--drop type if exists course_level cascade;
--
--create type course_level as enum
--    ( 'G1'
--    , 'G2'
--    , 'A'
--    )
--;

drop table if exists course cascade;

create table course
    ( code text primary key
    , credits real not null
    , level text not null
    , name text not null
    , comment text not null
    , year int
    , updated timestamptz not null default now()
    )
;

drop index if exists course_on_name_lower;
create index course_on_name_lower on course (lower(name));

drop table if exists ceq cascade;

create table ceq
    ( primary key (code, period)
    , code text not null references course
    , period text not null
    , students_reg int not null
    , students_pass int not null
    , students_resp int not null
    , satisfaction_mean int not null
    , satisfaction_stddev int not null
    , relevancy_mean int not null
    , relevancy_stddev int not null
    , url text not null
    , updated timestamptz not null default now()
    )
;

drop index if exists ceq_on_code;
create index ceq_on_code on ceq (code);
