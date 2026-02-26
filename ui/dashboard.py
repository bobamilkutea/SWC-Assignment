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
                "Average": student.average if student.average else 0.0,
                "Distinction": student.distinction_status,
                "Graduation Eligible": student.graduation_eligibility,
                "Performance": student.predict_performance()
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
                )
            }
        )
    else:
        st.info("No students found. Please add students to begin tracking.")
