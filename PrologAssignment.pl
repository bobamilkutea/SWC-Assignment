:- use_module(library(http/json)).

% ------------------------------------------------------------------
% Helper predicates
% ------------------------------------------------------------------

subset([], _).
subset([H|T], L) :- member(H, L), subset(T, L).

% Determine if a student is eligible for graduation based on program
% requirements and completed courses from main_database.json.
graduation_eligible(Programs, StudentDict, Eligible) :-
    ProgramCode = StudentDict.program,
    Completed   = StudentDict.completed_courses,
    % find the matching program
    member(Prog, Programs),
    ProgramCode = Prog.program_code,
    Required = Prog.required_courses,
    (  subset(Required, Completed)
    -> Eligible = true
    ;  Eligible = false
    ).

% Compute recommended courses: courses whose prerequisites are satisfied
% but which the student has not yet completed.
recommended_courses(Courses, StudentDict, Recommended) :-
    Completed = StudentDict.completed_courses,
    findall(Code,
      ( member(C, Courses),
        Code = C.course_code,
        \+ member(Code, Completed),
        PreReqs = C.prerequisites,
        subset(PreReqs, Completed)
      ),
      Recommended).

% Build a single student result dict for JSON output
student_result(Programs, Courses, StudentDict, Result) :-
    graduation_eligible(Programs, StudentDict, Eligible),
    recommended_courses(Courses, StudentDict, Recommended),
    Result = _{
        id: StudentDict.id,
        graduation_eligible: Eligible,
        recommended_courses: Recommended
    }.

% ------------------------------------------------------------------
% Entry point called from Python:
%   swipl -f PrologAssignment.pl -g run_query,halt.
% Produces JSON like:
% {
%   "students": [
%     { "id": 101, "graduation_eligible": false, "recommended_courses": ["CS201"] }
%   ]
% }
% ------------------------------------------------------------------
run_query :-
    open('main_database.json', read, Stream),
    json_read_dict(Stream, Data),
    close(Stream),
    Programs = Data.programs,
    Courses  = Data.courses,
    Students = Data.students,
    maplist(student_result(Programs, Courses), Students, ResultStudents),
    json_write_dict(current_output, _{students: ResultStudents}),
    nl.