#' survey 
#' 
#' @title survey 
#' 
#' @description The data was collected by applying a survey to a sample of university students.
#' 
#' @name survey 
#' @usage survey
#' 
#' @format  A data frame (tibble) with 800 observations and 66 variables, which are described below:
#' 
#' \describe{
#'  \item{\code{Observation}}{Student.}
#'  \item{\code{ID}}{Identification code.}
#'  \item{\code{Gender}}{Gender of the student, 1 = Female; 2 = Male.}
#'  \item{\code{Like}}{What do you do most often in your free time? 1 = Network (Check social networks);  2 = TV (Watch TV).}
#'  \item{\code{Age}}{Age of the student (in years), Numeric vector  from 12.0 to 30.0}
#'  \item{\code{Smoke}}{Do you smoke? 0 = No; 1 = Yes.}
#'  \item{\code{Height}}{Height of the student (in meters), Numeric vector from 1.50 to 1.90.}
#'  \item{\code{Weight}}{Weight of the student (in kilograms), numeric vector from 49 to 120.}
#'  \item{\code{BMI}}{Body mass index of the student  (kg/m^2), numeric vector from 14 to 54.}
#'  \item{\code{School}}{Type of school students come from, 1 = Private; 2 = Public.}
#'  \item{\code{SES}}{Socio-economic stratus of the student, 1 = Low; 2 = Medium; 3 = High.}
#'  \item{\code{Enrollment}}{What was your type  funding to study at the university? 1 = Credit; 2 = Scholarship; 3 = Savings.}
#'  \item{\code{Score}}{Percentage of success in a certain test, numeric vector from 0 to 100\%}
#'  \item{\code{MotherHeight}}{Height of the mother of the student (in meters), numeric vector 1 = Short; 2 = Normal; 3 = Tall.}
#'  \item{\code{MotherAge}}{Age of the mother of the student (in years), numeric vector from 39 to 89.}
#'  \item{\code{MotherCHD}}{Has your mother had coronary heart disease? 0 = No; 1 = Yes.}
#'  \item{\code{FatherHeight}}{Height of the father of the student (in meters), numeric vector 1 = Short; 2 = Normal; 3 = Tall.}
#'  \item{\code{FatherAge}}{Age of the father of the student (in years), numeric vector from 39 to 89}
#'  \item{\code{FatherCHD}}{Has your fatner had coronary heart diseasea, 1 = No; 2 = Yes.}
#'  \item{\code{Status}}{Student's academic status at the end of the previous semester, 1 = Distinguished; 2 = Normal; 3 = Regular.}
#'  \item{\code{SemAcum}}{Average of all final grades in the previous semester, numeric vector from 0.0  to 5.0}
#'  \item{\code{Exam1}}{First exam taken last semester, numeric vector from 0.0  to 5.0}
#'  \item{\code{Exam2}}{Second exam taken last semester, numeric vector from 0.0  to 5.0}
#'  \item{\code{Exam3}}{Third exam taken last semester, numeric vector from 0.0  to 5.0}
#'  \item{\code{Exam4}}{Last exam taken last semester, numeric vector from 0.0  to 5.0}
#'  \item{\code{ExamAcum}}{Sum of the four exams mentioned above, numeric vector from 0.0  to 5.0}
#'  \item{\code{Definitive}}{Average of the four exams mentioned above, numeric vector from 0.0  to 5.0}
#'  \item{\code{Expense}}{Average of your monthly expenses (in 10 thousand Colombian pesos), numeric vector from 23.0 to 90.0}
#'  \item{\code{Income}}{Father's monthly income (in millions of Colombian pesos), numeric vector from 1.0 to 3.0}
#'  \item{\code{Gas}}{Value paid for gas service in the last month (in thousands of Colombian pesos), numeric vector from 15.0 to 28.0}
#'  \item{\code{Course}}{What type of virtual classes do you prefer? 1 = Virtual; 2 = Face-to-face.}
#'  \item{\code{Law}}{Opinion on a law, 1 = In disagreement; 2=Agree}
#'  \item{\code{Economic}}{How was your family's economy during the pandemic? 1 = Bad; 2 = Regular; 3 = Good.}
#'  \item{\code{Race}}{Does the student belong to an ethnic group? 1=None; 2= Ethnic}
#'  \item{\code{Region}}{Region of the country where the student comes from, 1 = North; 2 = Center; 3 = South.}
#'  \item{\code{EMO1}}{During this period of preventative isolation, you frequently become nervous or restless for no reason, 1 = Never, 2 = Rarely;  3 = Almost always; 4 = Always.}
#'  \item{\code{EMO2}}{During this period of preventative isolation, you are often irritable, 1 = Never, 2 = Rarely;  3 = Almost always; 4 = Always.}
#'  \item{\code{EMO3}}{During this period of preventive isolation, you are often sad or despondent, 1 = Never, 2 = Rarely;  3 = Almost always; 4 = Always }
#'  \item{\code{EMO4}}{During this period of preventive isolation, you are often easily frightened, 1 = Never, 2 = Rarely;  3 = Almost always; 4 = Always}
#'  \item{\code{EMO5}}{During this period of preventative isolation, you often have trouble thinking clearly, 1 = Never, 2 = Rarely;  3 = Almost always; 4 = Always}
#'  \item{\code{GOAL1}}{I am concerned that I may not be able to understand the contents of my subjects this semester as thoroughly as I would like, 1 = Strongly agree; 2 = Disagree;  3 = Undecided; 4 = Agree; 5 = Strongly agree.}
#'  \item{\code{GOAL2}}{It is important for me to do better than other students in my subjects this semester, 1 = Strongly agree; 2 = Disagree;  3 = Undecided; 4 = Agree; 5 = Strongly agree.}
#'  \item{\code{GOAL3}}{I am concerned that I may not learn all that I can learn in my subjects this semester, 1 = Strongly agree; 2 = Disagree;  3 = Undecided; 4 = Agree; 5 = Strongly agree.}  
#'  \item{\code{Pre_STAT1}}{I like statistics, 1 = Strongly agree; 2 = Disagree;  3 = Undecided; 4 = Agree; 5 = Strongly agree.}  
#'  \item{\code{Pre_STAT2}}{I don't focus when I make problems statistics, 1 = Strongly agree; 2 = Disagree;  3 = Undecided; 4 = Agree; 5 = Strongly agree.}  
#'  \item{\code{Pre_STAT3}}{I don't understand statistics much because of my way of thinking, 1 = Strongly agree; 2 = Disagree;  3 = Undecided; 4 = Agree; 5 = Strongly agree.}    
#'  \item{\code{Pre_STAT4}}{I use statistics in everyday life, 1 = Strongly agree; 2 = Disagree;  3 = Undecided; 4 = Agree; 5 = Strongly agree.}    
#'  \item{\code{Post_STAT1}}{I like statistics, 1 = Strongly agree; 2 = Disagree;  3 = Undecided; 4 = Agree; 5 = Strongly agree.}  
#'  \item{\code{Post_STAT2}}{I don't focus when I make problems statistics, 1 = Strongly agree; 2 = Disagree;  3 = Undecided; 4 = Agree; 5 = Strongly agree.}  
#'  \item{\code{Post_STAT3}}{I don't understand statistics much because of my way of thinking, 1 = Strongly agree; 2 = Disagree;  3 = Undecided; 4 = Agree; 5 = Strongly agree.}    
#'  \item{\code{Post_STAT4}}{I use statistics in everyday life, 1 = Strongly agree; 2 = Disagree;  3 = Undecided; 4 = Agree; 5 = Strongly agree.}    
#'  \item{\code{Pre_IDARE1}}{I feel calm, 1=Nothing; 2= Little; 3= Quite a bit; 4= A lot.}  
#'  \item{\code{Pre_IDARE2}}{I feel safe, 1=Nothing; 2= Little; 3= Quite a bit; 4= A lot.}  
#'  \item{\code{Pre_IDARE3}}{I feel nervous, 1=Nothing; 2= Little; 3= Quite a bit; 4= A lot.}    
#'  \item{\code{Pre_IDARE4}}{I'm stressed, 1=Nothing; 2= Little; 3= Quite a bit; 4= A lot.}      
#'  \item{\code{Pre_IDARE5}}{I am comfortable, 1=Nothing; 2= Little; 3= Quite a bit; 4= A lot.}  
#'  \item{\code{Post_IDARE1}}{I feel calm, 1=Nothing; 2= Little; 3= Quite a bit; 4= A lot.}  
#'  \item{\code{Post_IDARE2}}{I feel safe, 1=Nothing; 2= Little; 3= Quite a bit; 4= A lot.}  
#'  \item{\code{Post_IDARE3}}{I feel nervous, 1=Nothing; 2= Little; 3= Quite a bit; 4= A lot.}    
#'  \item{\code{Post_IDARE4}}{I'm stressed, 1=Nothing; 2= Little; 3= Quite a bit; 4= A lot.}      
#'  \item{\code{Post_IDARE5}}{I am comfortable, 1=Nothing; 2= Little; 3= Quite a bit; 4= A lot.}  
#'  \item{\code{PSICO1}}{I feel good, 1=Almost never; 2= Sometimes; 3= Frequently; 4= Almost always.}  
#'  \item{\code{PSICO2}}{I get tired quickly, 1=Almost never; 2= Sometimes; 3= Frequently; 4= Almost always.}  
#'  \item{\code{PSICO3}}{I feel like crying, 1=Almost never; 2= Sometimes; 3= Frequently; 4= Almost always.}    
#'  \item{\code{PSICO4}}{I would like to be as happy as others seem to be, 1=Almost never; 2= Sometimes; 3= Frequently; 4= Almost always.}      
#'  \item{\code{PSICO5}}{I lose opportunities for not being able to decide quickly, 1=Almost never; 2= Sometimes; 3= Frequently; 4= Almost always.}  
#' }
#' 
#' 
#' @examples
#'   # data(survey)
#'   # maybe str(survey) ; plot(survey) ...
#' @keywords data
NULL