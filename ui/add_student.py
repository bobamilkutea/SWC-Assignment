import streamlit as st

def render_add_student(system):
    st.markdown("## ➕ Add New Student")
    st.caption("Register a new student into the university database.")

    with st.form("add_student_form", clear_on_submit=True):
        
        st.markdown("### 📝 Student Details")
        
        col1, col2 = st.columns(2)
        
        with col1:
            student_id = st.number_input(
                "Student ID", 
                min_value=1, 
                step=1,
                help="Unique identifier for the student"
            )
            name = st.text_input(
                "Full Name",
                placeholder="e.g. Ahmad Ali"
            )
            program = st.selectbox(
                "Academic Program", 
                ["CS", "IT", "SE", "DS"],
                help="Select the student's major"
            )

        with col2:
            completed_courses = st.text_area(
                "Completed Courses",
                placeholder="e.g. CS101, CS102, MATH201",
                help="Enter course codes separated by commas"
            )
            grades = st.text_area(
                "Grade Records",
                placeholder="e.g. CS101:85, CS102:90",
                help="Format: CourseCode:Grade (separated by commas)"
            )

        st.markdown("---")
        submitted = st.form_submit_button("Register Student", use_container_width=True)

        if submitted:
            if not student_id or not name:
                st.error("Please fill in all required fields (ID and Name).")
                return

            # Check for duplicate ID
            if any(s.id == int(student_id) for s in system.students):
                st.error(f"Student ID {student_id} already exists in the database.")
                return

            try:
                # Parse inputs
                completed_courses_list = [c.strip() for c in completed_courses.split(',')] if completed_courses else []
                
                grades_dict = {}
                if grades:
                    for pair in grades.split(','):
                        if ':' in pair:
                            course, grade = pair.split(':')
                            grades_dict[course.strip()] = float(grade.strip())
                
                # Add to system
                system.add_student_to_database(
                    int(student_id),
                    name,
                    program,
                    completed_courses_list,
                    grades_dict
                )
                
                # Force reload to update state
                st.session_state.initialized = False
                st.success(f"✅ Student {name} (ID: {student_id}) added successfully!")
                st.balloons()
                
            except ValueError:
                st.error("Invalid input format. Please check your grades format (e.g. CS101:85).")
            except Exception as e:
                st.error(f"An unexpected error occurred: {str(e)}")
