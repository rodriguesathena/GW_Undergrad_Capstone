#%%
import json
import os
import shutil

#%%

def save_to_json(data, output_file_path):
    with open(output_file_path, 'w') as output_file:
        json.dump(data, output_file, indent=2)

semester2code = { "sp":"01", "spr":"01", "spring":"01", "su":"02", "sum":"02", "summer":"02", "fa":"03", "fall":"03"}
thisfilename = os.path.basename(__file__) # should match _ver for version, ideally 3-digit string starting as "000", up to "999"

data_to_save = \
    {
        # -----------------------------------------------------------------------------------------------------------------------
        "Version":
            """000""",
        # -----------------------------------------------------------------------------------------------------------------------
        "Year":
            """2024""",
        # -----------------------------------------------------------------------------------------------------------------------
        "Semester":
            """Spring""",
        # -----------------------------------------------------------------------------------------------------------------------
        "project_name":
            """One Hundred Years of Women in Politics: 1900-2010""",
        # -----------------------------------------------------------------------------------------------------------------------
        "Objective":
            """ 
            The goal of this project is to analyze women's progression internationally in the political realm over 100 years (1900-2000).
            This will aid in understanding how, over time, womes access to civil liberties, participation in society, and representation in politics have changed. 
            By tracking whether women have the right to vote in each nation in line with the political empowerment index it can be determined if there is a correlation between the two. 
            It is important to understand the struggle for women's rights and gender equality in history to ensure they are not taken for granted or chipped away. 
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Dataset":
            """
            The current datasets, Women's Empowerment Index and Womens Universal Right to Vote, come from Ourworldindata.
	
	        The Women's Political Empowerment Index (WPEI) is calculated by V-Dem Institute, a think tank that studies democracy and other government regimes around the world. 
            A combination of the Women Civil Liberties Index, Women Civil Society Participation Index, and Women Political Participation Index, the Women Political Empowerment Index provides a quantitative analysis of women's rights and aids in understanding the development or destruction of democracy equal civil rights is seen as a necessity to transition to democracy. 
            The data breaks down the WPEI by continent and country from 1989 to 2022 tracking the absolute and relative changes of the central estimates and both bounds.
        	
            The Women's Universal Right to Vote dataset tracks whether womens suffrage is present in a country using a system of 0 and 1 (0 for no, 1 for yes). 
            This data is broken down by country and records the right to vote from 1789 to 2022.
	       
            Because both of the datasets span the same range of time, it should be easy to compare and combine datasets without loss of data or confusion. The other datasets that created the Women's Empowerment Index are being looked for to be included for better analysis

           """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Rationale":
            """
            This project will aid in understanding the history and progression of gender equality while also showcasing areas in need of greater support.            
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Approach":
            """
            I plan on approaching this capstone through several steps.  

            1. Compare the components of the Women Political Empowerment Index using various tests
            2. Map the Right to Vote over time
            3. Map the Women's Empowerment Index over time
            4. Analyse and test the relationship between the WPEI and years of change (years before and after women received the right to vote)
            5. Breakdown data and create graphics that summarize the 100 years into three sections: pre-suffrage, suffrage, post-suffrage
 
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Timeline":
            """
            This a rough time line for this project:  

            Feb. 12: Mapping Right to Vote over time
	        Feb. 19: Mapping Women Empowerment Index over time
	        Mar. 4: comparing indices (components of WPEI)
	        Mar. 18: Testing relationships
	        Apr. 1: Pre-suffrage, suffrage, post-suffrage graphics
	        Apr. 9: Finishing Touches
	        Apr. 15: Posterboard
	        Apr. 15: Presentation
	        May 1: Video Presentation and Report
  
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Expected Number Students":
            """
            I will be working alone on this project.  
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Possible Issues":
            """
            The challenge is on correlation testing (using the proper tests for the data) and remembering teh historical context of the time.
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Proposed by": "Athena Rodrigues",
        "Proposed by email": "arodrigues@gwu.edu",
        "instructor": "Edwin Lo",
        "instructor_email": "edwinlo@gwu.edu",
        "github_repo": "https://github.com/rodriguesathena/24Spr_ARodrigues_Capstone.git",
        # -----------------------------------------------------------------------------------------------------------------------
    }
os.makedirs(
    os.getcwd() + f'{os.sep}Proposals{os.sep}{data_to_save["Year"]}{semester2code[data_to_save["Semester"].lower()]}{os.sep}{data_to_save["Version"]}',
    exist_ok=True)
output_file_path = os.getcwd() + f'{os.sep}Proposals{os.sep}{data_to_save["Year"]}{semester2code[data_to_save["Semester"].lower()]}{os.sep}{data_to_save["Version"]}{os.sep}'
save_to_json(data_to_save, output_file_path + "input.json")
shutil.copy(thisfilename, output_file_path)
print(f"Data saved to {output_file_path}")