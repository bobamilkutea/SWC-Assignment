% Facts for students
student('Arief', s001).
student('Amsyar', s002).
student('Syamir', s003).

% Facts for enrollments (completed courses)
enrolled(s001, 'MMC3113').
enrolled(s001, 'SWC3524').
enrolled(s001, 'ITC3084').

enrolled(s002, 'MMC3113').
enrolled(s002, 'ITC3084').

enrolled(s003, 'MMC3113').
enrolled(s003, 'SWC3524').
enrolled(s003, 'ITC3084').
enrolled(s003, 'SWC2623').

% Facts for prerequisites
prerequisite('SWC3524', 'MMC3113').
prerequisite('SWC2623', 'MMC3113').

% Check if student has completed a course
completed(StudentID, CourseCode) :- enrolled(StudentID, CourseCode).

% Check if eligible to enroll (all prereqs completed)
eligible(StudentID, CourseCode) :-
    findall(Prereq, prerequisite(CourseCode, Prereq), Prereqs),
    forall(member(Prereq, Prereqs), completed(StudentID, Prereq)).

% Recommend courses (eligible but not enrolled)
recommend(StudentID, CourseCode) :-
    prerequisite(CourseCode, _),
    eligible(StudentID, CourseCode),
    \+ enrolled(StudentID, CourseCode).

% Eligible for graduation (completed all required courses)
required_course('MMC3113').
required_course('SWC3524').
required_course('ITC3084').

eligible_for_graduation(StudentID) :-
    forall(required_course(Course), completed(StudentID, Course)).