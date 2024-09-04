Project Summary:
Our project, "Fake News Detection," is an interactive web application built using Shiny, a web application framework in R. The application aims to analyze news articles and user-provided text to determine whether the content is likely to be 'real' or 'fake'. Key functionalities include analyzing uploaded datasets, visualizing key metrics, and evaluating user-provided text.
How It Works:
1.	File Upload and Analysis:
o	Users can upload datasets in various formats (.csv, .xlsx, .xls, .txt).
o	The app calculates summary statistics (e.g., percentage of 'real' and 'fake' news).
o	Visualizations include boxplots for exclamation marks, question marks, and a word count boxplot.
2.	User Text Analysis:
o	Users can input text directly for analysis.
o	Sentiment analysis (syuzhet package) determines the likelihood of the text being 'real' or 'fake'.
o	Feedback on text sentiment and reliability is provided.
3.	Interactive Interface:
o	User-friendly interface with tabs for different analyses and a sidebar for file upload and text input.
o	Includes explanatory text to guide users effectively.
Code Implementation:
•	UI (User Interface):
o	Designed using fluidPage, titlePanel, sidebarLayout, and mainPanel.
o	Implemented functions: fileInput, textAreaInput, actionButton, plotOutput, and renderPlot.
•	Server Logic:
o	Handles reactive data reading (reactive and reactiveFileReader), data validation (validate), and output rendering.
o	Data analysis uses dplyr, tidyr for manipulation, syuzhet for sentiment analysis, and ggplot2 for plotting.
Conclusion:
Our project, "Fake News Detection," demonstrates data and sentiment analysis techniques for identifying fake news. The Shiny app provides an accessible platform for dataset analysis, news content evaluation, and text credibility assessment.
Acknowledgments:
Special thanks to the developers of Shiny, tidyverse, and other R packages used in this project.
![image](https://github.com/user-attachments/assets/4a9fa0dc-7a4a-4a38-a868-b0ef367e58ee)
