:- use_module(library(http/json)).
% Or if using newer SWI-Prolog versions, you might prefer:
% :- use_module(library(json)).

% ------------------------------------------------------------------
% Helper predicates
% ------------------------------------------------------------------

subset([], _).
subset([H|T], L) :- member(H, L), subset(T, L).

% Determine if a student is eligible for graduation based on program
% requirements and completed courses from main_database.json.
% If the student's program is not found in the Programs list, we
% treat them as NOT eligible instead of failing the whole predicate.
graduation_eligible(Programs, StudentDict, Eligible) :-
    ProgramCode = StudentDict.program,
    Completed   = StudentDict.completed_courses,
    (   % try to find matching program
        member(Prog, Programs),
        ProgramCode = Prog.program_code
    ->  Required = Prog.required_courses,
        (  subset(Required, Completed)
        -> Eligible = true
        ;  Eligible = false
        )
    ;   % unknown program code -> not eligible, but still succeed
        Eligible = false
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

% ------------------------------------------------------------------
% Pretty console output (for screenshots / human-readable report)
% ------------------------------------------------------------------

yes_no(true,  'YES').
yes_no(false, 'NO').

print_student_report(Programs, Courses, StudentDict) :-
    Name = StudentDict.name,
    StudentID = StudentDict.id,
    Completed = StudentDict.completed_courses,
    graduation_eligible(Programs, StudentDict, Eligible),
    yes_no(Eligible, EligibleText),
    recommended_courses(Courses, StudentDict, Recommended),
    format("Name         : ~w~n", [Name]),
    format("Student ID   : ~w~n", [StudentID]),
    format("Courses      : ~w~n", [Completed]),
    format("Can Graduate : ~w~n", [EligibleText]),
    format("Recommended  : ~w~n", [Recommended]),
    format("------------------------------------~n~n", []).

% Entry point for a full report (similar style to your screenshot)
% Run:
%   swipl -f PrologAssignment.pl -g show_all_students,halt.
show_all_students :-
    open('main_database.json', read, Stream),
    json_read_dict(Stream, Data),
    close(Stream),
    Programs = Data.programs,
    Courses  = Data.courses,
    Students = Data.students,
    format("========== STUDENT REPORT ==========\n\n", []),
    forall(member(S, Students), print_student_report(Programs, Courses, S)).

% Optional: show one student by ID (handy for screenshots)
% Run:
%   swipl -f PrologAssignment.pl -g "show_student(102)",halt.
show_student(StudentID) :-
    open('main_database.json', read, Stream),
    json_read_dict(Stream, Data),
    close(Stream),
    Programs = Data.programs,
    Courses  = Data.courses,
    Students = Data.students,
    (   member(S, Students), S.id =:= StudentID
    ->  format("========== STUDENT REPORT ==========\n\n", []),
        print_student_report(Programs, Courses, S)
    ;   format("No student found with ID: ~w~n", [StudentID])
    ).