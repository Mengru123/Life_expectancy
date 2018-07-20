-- create LE_table that contains people in the cohort at the midyear
drop table if exists LE_table
create TEMPORARY table LE_table as
select *
from ref.patient_ranges as per
inner join
  (select
     to_date(concat(
                 to_char(generate_series(
                             min(extract(yr from lower(range))) :: integer,
                             max(extract(yr from upper(range))) :: integer), '9999'), '-07-01'),
             'YYYY-MM-dd') AS midyear
from ref.patient_ranges) as m
  on range @> m.midyear :: timestamp;

-- create LE_table_demon contains No.of people contribute to denominator by age,CLSC, and yr in cohort
-- group people who are 85+ (do it in R after checking whether need to group multiple years), & delete the people who are below 0-year-old
drop table if exists public.LE_table_denom
create table public.LE_table_denom as
  (select
     ct.dst,
     tem.age,
     /*case when tem.age >= 85
       then 85
         else tem.age end, */
     count(tem.fk_patient),
     tem.midyear
   from dim.g_ct2clsc as ct
     right join
     (SELECT
        extract(year from age(midyear, dob)) :: integer as age,
        le.fk_patient,
        le.fk_ct,
        le.midyear
      from LE_table as le
        left join ref.patient as p
          on le.fk_patient = p.id) as tem
       on ct.src = tem.fk_ct
   group by ct.dst, tem.midyear, tem.age/*case when tem.age >= 85 then 85 else tem.age end*/
  );
delete from public.LE_table_denom as led
where led.age <0;

select *
from LE_table_denom as led
order by led.age;

-- 31 cases where people are nor born when in the pophr cohort: probably due to wrong coding in RAMQ (dob2001 should be 1991, for example)
WITH age_midyr as
(SELECT
        extract(year from age(midyear, dob)) :: integer as age,
        age(le.midyear, p.dob) as age2,
        le.fk_patient,
        le.fk_ct,
        le.midyear,
        p.dob,
        le.range,
        p.id
      from LE_table as le
        left join ref.patient as p
          on le.fk_patient = p.id)
select *
from age_midyr
order by age_midyr.age;
-------------------------------------------------------------------------------------------
-- right join keep all fk_patient in right tbl(ref.deaths) & repeat for death date for each cohort period in patient_range
-- LE_num: 1016230 rows and 120824 unique fk_patient; ref.deaths:120824 rows of fk_patient(unique death)
drop table if exists public.LE_num
create table public.LE_num as
with dod_num as -- with age info
(select d.fk_patient, extract(year from age(d.date, p.dob)) as age, d.date from ref.deaths as d
  left join ref.patient as p
    on d.fk_patient = p.id )
select dod.fk_patient, le.range, le.fk_ct, dod.age,dod.date from dod_num as dod
  left join ref.patient_ranges as le
  on dod.fk_patient = le.fk_patient;

-- from LE_num find people died in one of their cohort period
-- Among 120824 deaths, 109389 died during cohort period & 11435 died out of cohort period (possible reason: ISQ delayed data)
-- New LE_num table has 109389 rows, with no repeat fk_patient records
delete from public.LE_num as len
where NOT len.range @> len.date::timestamp;

-- create the final LE numerator table
drop table if exists public.LE_table_num
create table public.LE_table_num as
select len.age, extract(year from len.date) as death_yr, g.dst, count(len.fk_patient) from public.LE_num len
right join dim.g_ct2clsc g
  on len.fk_ct = g.src
group by len.age, death_yr, g.dst;

-- export dataset using dump & clear the data in the database
drop table if exists public.LE_num;
drop table if exists public.LE_table_num;
drop table if exists public.LE_table_denom;

