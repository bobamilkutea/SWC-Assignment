import streamlit as st
import pandas as pd

def render_at_risk(system):
    st.markdown("## ⚠️ At-Risk Students")
    st.caption("Students requiring academic intervention (Average < 50).")

    at_risk_students = system.get_at_risk_students()

    if at_risk_students:
        
        # Summary Metric
        st.metric("Total At-Risk Students", len(at_risk_students), delta=f"{len(at_risk_students)} Students", delta_color="inverse")
        
        st.markdown("### 📋 Detailed List")

        # Prepare Data for Table
        table_data = []
        for student in at_risk_students:
            table_data.append({
                "ID": student.id,
                "Name": student.name,
                "Program": student.program,
                "Average": student.average,
                "Status": "⚠️ Critical"
            })
            
        df = pd.DataFrame(table_data)

        # Interactive Table
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
                "Status": st.column_config.TextColumn(
                    "Risk Status",
                    width="small"
                )
            }
        )
        
        st.info("💡 Recommendation: Schedule academic counseling sessions for these students.")

    else:
        st.success("✅ Great news! No students are currently at risk.")
