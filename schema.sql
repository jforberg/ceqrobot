--drop type if exists course_level cascade;
--
--create type course_level as enum
--    ( 'G1'
--    , 'G2'
--    , 'A'
--    )
--;

drop table if exists queue cascade;
create table queue
    ( primary key (type, ref)
    , type text not null
    , ref text not null
    , meta text
    , prio double precision not null
    , updated timestamptz not null default now()
    )
;

drop index if exists queue_on_prio;
create index queue_on_prio on queue (prio);

drop table if exists programme cascade;
create table programme
    ( code text primary key
    , name text not null
    )
;

drop table if exists course cascade;
create table course
    ( code text primary key
    , credits double precision not null
    , level text not null
    , name text not null
    , updated timestamptz not null default now()
    )
;

drop index if exists course_on_name_lower;
create index course_on_name_lower on course (lower(name));

drop table if exists course_relation;
create table course_relation
    ( primary key (code, programme, masters, validYear)
    , code text not null references course
    , programme text not null references programme
    , type text not null
    , masters text not null
    , comment text not null
    , programmeYear int not null
    , validYear int not null
    , periodical boolean not null
    , lp1 boolean not null
    , lp2 boolean not null
    , lp3 boolean not null
    , lp4 boolean not null
    , constraint periodical_no_lp check
        (not periodical or (not lp1 and not lp2 and not lp3 and not lp4))
    , updated timestamptz not null default now()
    )
;

drop index if exists course_relation_on_code;
create index course_relation_on_code on course_relation (code);
drop index if exists course_relation_on_programme_masters;
create index course_relation_on_programme_masters on course_relation (programme, masters);
drop index if exists course_relation_on_validYear;
create index course_relation_on_validYear on course_relation (validYear);

drop table if exists ceq cascade;
create table ceq
    ( primary key (code, year, semester, period)
    , code text not null references course
    , year int not null
    , semester text not null
    , period int not null
    , url text not null
    , registered int
    , passed int
    , responded int
    , quality int
    , goals int
    , understanding int
    , workload int
    , relevance int
    , satisfaction int
    , updated timestamptz not null default now()
    )
;

drop index if exists ceq_on_code;
create index ceq_on_code on ceq (code);

drop table if exists masters cascade;
create table masters
    ( primary key (programme, code)
    , programme text not null references programme
    , code text not null
    , name text not null
    , updated timestamptz not null default now()
    )
;

