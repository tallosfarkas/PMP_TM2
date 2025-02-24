
# **PMP_TM2**

This repository contains the files and data necessary for working on equity and portfolio-related strategies using R. Follow the instructions below to set up the repository in your RStudio environment and start working with the provided data.

---

# **If you don't have git installed, first install it from here:**
https://git-scm.com/downloads


## **Getting Started with This Repository in RStudio**

### **Step 1: Clone the Repository**
1. Open RStudio.
2. Go to **File > New Project > Version Control > Git**.
3. In the dialog box:
   - **Repository URL**: Paste the URL of this repository:  
     `https://github.com/tallosfarkas/PMP_TM2.git`
   - **Project Directory Name**: Choose a folder name for the project.
   - **Create Project As Subdirectory Of**: Select the folder where you'd like to save the project.
4. Click **Create Project**.

This will clone the repository and set it up as a new RStudio project.

---

### **Step 2: Open the Project**
1. After cloning, RStudio will open the project automatically.
2. If it doesn't, navigate to the folder you cloned and double-click the `.Rproj` file.

---

### **Step 3: Install Required Libraries**
Ensure that the necessary R libraries are installed.

---



## **Git Workflow: Collaborating and Version Control**

### **When Starting Work**
Before making changes to the files, ensure you are working with the latest version of the repository:
1. Open the RStudio Terminal or use an external terminal.
2. Run the following command to fetch the latest changes:
   ```bash
   git pull
   ```
   This updates your local repository with any new commits from the remote repository.

---

### **Making and Saving Changes**
1. After working on the file(s) and saving your changes in RStudio:
   - Stage the changes:
     ```bash
     git add .
     ```
   - Commit the changes with a descriptive message:
     ```bash
     git commit -m "Your descriptive commit message here"
     ```

---

### **When Finishing Work**
1. Push your changes to the remote repository:
   ```bash
   git push
   ```
   This ensures your changes are shared with others using the repository.

---

### **Example Workflow**
Here’s a full example workflow:
```bash
# Pull the latest changes before starting work
git pull

# Make changes to your files
# Then stage all changes
git add .

# Commit with a descriptive message
git commit -m "Updated load_and_e_ret.R with additional analysis"

# Push the changes to the remote repository
git push
```

---

### **Repository Structure**
- etc.
- etc.
- etc.

---

If you have questions or need help, feel free to reach out via GitHub Issues. Happy coding! 🎉
