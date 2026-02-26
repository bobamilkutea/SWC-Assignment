import streamlit as st

def render_top_performer(system):
    st.markdown("## 🏆 Top Performer")
    st.caption("Recognizing academic excellence.")

    top_student = system.get_top_performer()

    if top_student:
        st.markdown(f"""
        <div class="custom-card">
            <div style="font-size: 4rem; margin-bottom: 1rem;">🥇</div>
            <h1 style="color: #047857; margin-bottom: 0.5rem;">{top_student.name}</h1>
            <h3 style="color: #065f46; font-weight: 500;">ID: {top_student.id}</h3>
            <div style="background-color: #ecfdf5; padding: 0.5rem 1rem; border-radius: 999px; display: inline-block; margin: 1rem 0;">
                <span style="color: #059669; font-weight: 600;">{top_student.program}</span>
            </div>
            <div style="margin-top: 2rem;">
                <h1 style="color: #064e3b; font-size: 3.5rem; font-weight: 700; line-height: 1;">
                    {top_student.average:.2f}
                </h1>
                <p style="color: #64748b; font-size: 1.1rem; margin-top: 0.5rem;">Average Grade</p>
            </div>
        </div>
        """, unsafe_allow_html=True)
        
        # Additional Details
        with st.expander("View Detailed Performance"):
            st.json({
                "ID": top_student.id,
                "Name": top_student.name,
                "Program": top_student.program,
                "Average": top_student.average,
                "Distinction": top_student.distinction_status,
                "Graduation Eligible": top_student.graduation_eligibility
            })

    else:
        st.info("No top performer found. Add students with grades to calculate performance.")
