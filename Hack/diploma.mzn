int: ncourses;
int: total_hcredit_spec = 100; %600;
int: total_ahcredit_spec = 0;
int: max_extra_hcredit = 15;

array [courses] of int: bhcredits;
array [courses] of int: ahcredits;
array [courses] of 0..1: obligatory;
array [courses] of int: value;

set of int: courses = 1..ncourses;

var set of courses: selected;

var int: total_bhcredits = sum (c in selected) (bhcredits[c]);
var int: total_ahcredits = sum (c in selected) (ahcredits[c]);
var int: total_hcredits = total_bhcredits + total_ahcredits;
var int: total_value = sum (c in selected) (value[c]);

constraint total_hcredits >= total_hcredit_spec /\
           total_hcredits - total_hcredit_spec < max_extra_hcredit;
constraint total_ahcredits >= total_ahcredit_spec;
%constraint forall (c in courses) (obligatory[c] == 1 -> c in selected);

solve maximize total_value;

output [show(selected)];
