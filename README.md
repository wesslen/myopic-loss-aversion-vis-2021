Paper (preprint): [Effect of uncertainty visualizations on myopic loss aversion and the equity premium puzzle in retirement investment decisions](https://arxiv.org/abs/2107.02334)

![](https://img.shields.io/github/license/wesslen/myopic-loss-aversion-vis-2021?style=plastic)
![](https://img.shields.io/github/repo-size/wesslen/myopic-loss-aversion-vis-2021?style=plastic)

![favicon-16x16](https://github.com/heroku/favicon/raw/master/favicon.iconset/icon_16x16.png) Experiment app: [https://retirement-study-1.herokuapp.com](https://retirement-study-1.herokuapp.com)

[![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-blue?logo=gitpod)](https://gitpod.io/#github.com/wesslen/myopic-loss-aversion-vis-2021/)

[![Launch binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/wesslen/myopic-loss-aversion-vis-2021/HEAD?urlpath=rstudio)

```
📦 myopic-loss-aversion-vis-2021                                                                       
┣━━ 00-design-space.R           - exploration for design space of possible vis treatments                                                    
┣━━ 01-get-data.Rmd             - fetch data from MongoDB for analysis                                          
┣━━ 01-simulated-returns.R/Rmd  - simulate returns used in the study
┣━━ 02-results.Rmd              - analyze experiment (non-model) results
┣━━ 03-validate-models.Rmd      - fit and validate/criticize models
┣━━ 04-analyze-modeling.Rmd     - analyze model
┣━━ install.R                   - install R packages
┣━━ run-incentives.R            - code to run incentives
┣━━ 🖥 experiment-reactjs-app
┃   ┗━━ server.js               - experiment study server file
┣━━ 💬 qualitative study
┃   ┣━━ feedback.csv            - user feedback data
┃   ┗━━ topicModeling.ipynb     - user feedback topic modeling
┣━━ 📐 experiment-design
┃   ┣━━ experiment-wireframe-retirement.docx - experiment wireframe
┃   ┗━━ Task2-Instructions.docx - Task 2 unique instructions per treatment
┣━━ 📈 models
┃   ┗━━ fit1-6.rda              - models considered and evaluated
┗━━ 👨‍💻 data
    ┗━━ final_responses.csv     - experiment responses
```
