# Indian Medical Practitioners Recommendation Engine


This report aims to provide a comprehensive rating system for healthcare providers in the various medical specialties. The site Practo has been used as the data source for generating medical professionals’ data. Other data sources like just dial, and Medical Council website has been used for data confirmation.
Doctors education data, experience, hospitals working for, awards received or thesis submission and user reviews (feedback) has been scraped from Practo. Z score has been calculated to standardize each parameter in the doctors’ dataset. R score is assigned to each doctor for each parameter. Recommendation engine is built based on the R scores of each parameter. Once the average R score is achieved for individual doctor percentile ranking is applied on the whole data set to get the exact recommendation score for the doctor in selected location and specialization.
The parameters used to build this recommendation engine are Experience of doctor, hospital to which doctor is associated with, Education/specialization, awards or thesis submitted and on user feedback or review. Doctor’s registration number is verified against medical council registry.
Cardiologist, Gynaecologist and General Physician data has been extracted from Practo, top hospitals list has been extracted from Just dial and medical council site is used to verify the doctor registration details.
Recommendation Engine will allow user to enter the specialization and location and the result will be displayed based on highest to lowest rating.

Methodology:
R programming is used to build the recommendation engine. R selenium package was used to scrape all the reviews for all doctors from Practo. It is used for verification of registration number against Indian medical council registry as well.
text2vec package stop words along with stop words file created manually using the word cloud analysis is used to clean the words which are not significant for sentiment analysis. NLP package was used to classify the reviews (word tokenisation). text2vec package functions have been used to create vocabulary. Once the vocabulary is created Document term matrix (DTM) is constructed from vocabulary. A vector is created to identify non empty documents in DTM.
Qdap dictionary has been used to identify the average, negative and positive polarity. Average polarity score is considered for each doctors and used in recommendation engine. Recent review polarity score has been considered wherever it is applicable and in case of unavailability, overall reviews are considered.

R score is calculated as below for Parameters: Experience, Hospitals associated with(taking into consideration awards and recognitions), User review polarity score, .and Education
R score for each parameter = ZScore (each doctor for each parameter)+C/D
C and D are constants defined as such that the negative Z scores are taken care. The R score lies near to 25-30. R scores are derived for each parameter and doctor. R scores for 4 parameters are averaged. Once the R score average is obtained for each doctor percent rank will be deduced for each doctor average R score. Percent rank is applied for Average R score data. Ranking in percentages will be assigned to each doctor.

