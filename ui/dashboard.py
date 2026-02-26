import streamlit as st
import pandas as pd

def render_dashboard(system):
    st.markdown("## 📊 Student Dashboard")
    st.caption("Overview of student academic performance")

    students = system.students

    if students:
        # Metrics Row
        col1, col2, col3, col4 = st.columns(4)

        with col1:
            st.metric("Total Students", len(students))

        with col2:
            distinction_count = len([s for s in students if s.distinction_status])
            st.metric("Distinction", distinction_count)

        with col3:
            grad_eligible = len([s for s in students if s.graduation_eligibility])
            st.metric("Graduation Eligible", grad_eligible)

        with col4:
            at_risk = len(system.get_at_risk_students())
            st.metric("At-Risk", at_risk)

        st.markdown("---")
        st.markdown("### 👥 Student Records")

        # Prepare Data for Interactive Table
        table_data = []
        for student in students:
            table_data.append({
                "ID": student.id,
                "Name": student.name,
                "Program": student.program,
                "Average": student.average if student.average is not None else 0.0,
                "Distinction": student.distinction_status,
                "Graduation Eligible": student.graduation_eligibility,
                "Performance": student.predict_performance(),
                "Recommended Courses": ", ".join(student.recommended_courses) if student.recommended_courses else "None"
            })
        
        df = pd.DataFrame(table_data)

        # Interactive Dataframe Configuration
        st.dataframe(
            df,
            use_container_width=True,
            hide_index=True,
            column_config={
                "ID": st.column_config.NumberColumn(
                    "Student ID",
                    format="%d",
                    width="small"
                ),
                "Name": st.column_config.TextColumn(
                    "Full Name",
                    width="medium"
                ),
                "Program": st.column_config.TextColumn(
                    "Program",
                    width="small"
                ),
                "Average": st.column_config.ProgressColumn(
                    "Average Grade",
                    format="%.2f",
                    min_value=0,
                    max_value=100,
                    width="medium"
                ),
                "Distinction": st.column_config.CheckboxColumn(
                    "Distinction",
                    width="small"
                ),
                "Graduation Eligible": st.column_config.CheckboxColumn(
                    "Graduation Eligible",
                    width="small"
                ),
                "Performance": st.column_config.TextColumn(
                    "Performance Status",
                    width="medium"
                ),
                "Recommended Courses": st.column_config.TextColumn(
                    "Recommended Courses",
                    width="large"
                )
            }
        )

        # Section: Check specific student by ID
        st.markdown("---")
        st.markdown("### 🔍 Check Specific Student")
        selected_id = st.number_input(
            "Enter Student ID",
            min_value=0,
            step=1,
            format="%d",
            key="dashboard_search_id"
        )

        if selected_id:
            target = next((s for s in students if s.id == int(selected_id)), None)
            if target:
                st.markdown(f"#### Details for **{target.name}** (ID: {target.id})")
                col_a, col_b = st.columns(2)
                with col_a:
                    st.write(f"**Program:** {target.program}")
                    avg_text = f"{target.average:.2f}" if target.average is not None else "N/A"
                    st.write(f"**Average:** {avg_text}")
                    st.write(f"**Performance:** {target.predict_performance()}")
                with col_b:
                    st.write(f"**Distinction:** {'Yes' if target.distinction_status else 'No'}")
                    st.write(f"**Graduation Eligible:** {'Yes' if target.graduation_eligibility else 'No'}")
                    rec_text = ", ".join(target.recommended_courses) if target.recommended_courses else "None"
                    st.write(f"**Recommended Courses:** {rec_text}")

                st.markdown("**Completed Courses:**")
                st.write(", ".join(target.completed_courses) if target.completed_courses else "None")

                st.markdown("**Grades:**")
                if target.grades:
                    grade_rows = [{"Course": c, "Grade": g} for c, g in target.grades.items()]
                    st.dataframe(grade_rows, hide_index=True, use_container_width=True)
                else:
                    st.write("No grades recorded.")

                # Inline editor for adding new course and grade
                st.markdown("---")
                st.markdown("#### ✏️ Add Completed Course for This Student")
                with st.form(f"edit_student_{target.id}"):
                    col_c, col_d = st.columns(2)
                    with col_c:
                        new_course = st.text_input(
                            "New Course Code",
                            placeholder="e.g. CS201",
                        )
                    with col_d:
                        new_grade = st.text_input(
                            "Grade for New Course",
                            placeholder="e.g. 85",
                        )
                    submitted_edit = st.form_submit_button("Add Course & Recalculate")

                    if submitted_edit:
                        if not new_course or not new_grade:
                            st.error("Please provide both course code and grade.")
                        else:
                            try:
                                grade_value = float(new_grade)
                                system = st.session_state.system
                                system.update_student_courses(
                                    target.id,
                                    [new_course],
                                    {new_course: grade_value},
                                )
                                # Force system reload so Haskell/Prolog recompute averages
                                st.session_state.initialized = False
                                st.success(
                                    f"Added course {new_course} with grade {grade_value} "
                                    f"for {target.name}. Data will refresh on next load."
                                )
                            except ValueError:
                                st.error("Grade must be a valid number (e.g. 85 or 72.5).")
                            except Exception as e:
                                st.error(f"Failed to update student: {str(e)}")
            else:
                st.info("No student found with that ID.")
    else:
        st.info("No students found. Please add students to begin tracking.")
