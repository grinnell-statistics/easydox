#' Bacteria: Two-way Balanced Factorial Design
#'
#'Two students, Isaac and Courtney, sampled surfaces around their campus to analyze the preva- lence of bacteria. They compared different types of surfaces in both residential and academic buildings. Data were collected by wiping surfaces with a wet Q-tip and swabbing the result on a standard
#'nutrient agar plate. Locations were all tested on April 24, 2009 over the course of a two-hour period. The plates were incubated at 37°C for 48 hours before colony-forming units (CFUs) were counted as a measure of bacteria levels. Figure 4.5 shows several of the plates from this study. When CFUs exceeded 400 per plate, one fourth of the plate was counted and the total was calculated from that sample.
#'
#' @format
#' A data frame with 36 rows and 3 columns:
#' \describe{
#'   \item{Building}{Building name}
#'   \item{Location}{Location in each building}
#'   \item{Count}{Colony-forming units: a measure of bacteria level}
#' }
"Bacteria"

#' Popcorn: Three-way Balanced Factorial Design
#'
#' A dataset containing the pop rate experiments under different conditions
#'
#' @format
#' A data frame with 32 rows and 5 columns:
#' \describe{
#'   \item{Brand}{Brand name}
#'   \item{Microwave}{Microwave location}
#'   \item{Time}{Microwave time}
#'   \item{Unpopped..percent.}{Unpopped percent}
#'   \item{Poprate}{Popped percent}
#' }
"Popcorn"


#' Movies: Two-way Balanced Factorial Design
#'
#'The file Movies contains the ratings and genres of movies that came out in 2008.
#'This is simply an observational study, as clearly movie producers
#'do not try to create an equal number of  G, PG, and R movies each year.
#'Note:
#'\describe{
#'   \item{G}{Appropriate for people of all ages.}
#'   \item{PG-13}{Parents Strongly Cautioned under the age of 13.}
#'   \item{R}{Children Under 17 Require Accompanying Parent or Adult Guardian.}
#' }
#'
#' @format
#' A data frame with 210 rows and 4 columns:
#' \describe{
#'   \item{Movie}{Movie name}
#'   \item{Total.Gross}{Gross earnings}
#'   \item{Genre}{Genre of the movie}
#'   \item{Rating}{Rating of the movie}
#' }
"Movies"


#' Cholesterol: Three-way Balanced Factorial Design
#'
#'Cholesterol is a waxy substance found in blood and cell membranes.
#'All animals need some cholesterol in their system;
#'however, too much cholesterol can cause heart attacks and strokes.
#'A study was conducted to determine how exercise, diet, and three types of drugs impact cholesterol levels.
#'Seventy-two patients at a nearby hospital who had been diagnosed with high cholesterol
#'(a level greater than 240 milligrams per deciliter) consented to be in the study.
#'Each of the 72 patients was randomly assigned to a specific treatment, and after six months the patients’ cholesterol levels were measured again.
#'
#' @format
#' A data frame with 72 rows and 5 columns:
#' \describe{
#'   \item{Patient}{Patient number}
#'   \item{Exercises}{Exercise or not}
#'   \item{Diet}{Diet or not}
#'   \item{Drug}{Types of drugs}
#'   \item{Cholesterol}{Milligrams per deciliter: a measure of cholesterol level}
#' }
"Cholesterol"


#' Soda: Three-way Balanced Factorial Design
#'
#'Two students, Julie and Daphne, conducted an experiment to test the effects of soda type (Pepsi vs. 7-Up),
#'angle of the cup (cup flat on the table or slightly tipped),
#'and soda temperature (refrigerated at 5°C vs. room temperature at 21°C) on the
#'height of fizz produced when soda was poured out of a can into a cup.
#'For each of the 24 trials, these students poured a can of either Pepsi or 7-Up into a
#'clear cup and measured the peak fizz (in centimeters) produced, using a ruler on the outside of the cup.
#' @format
#' A data frame with 24 rows and 5 columns:
#' \describe{
#'   \item{Soda}{Pepsi or 7-Up}
#'   \item{Tilt}{Tipped or untipped}
#'   \item{Temp}{Room temp or cold}
#'   \item{Fizz}{Peak fizz produced in centimeters}
#'   \item{LogFizz}{Log of peak fizz produced in centimeters}
#' }
"Soda"


#' MemoryA: Two-way Balanced Factorial Design
#'
#'Michael Eysenck tested 100 subjects (50 people between the ages of 55 and 65, 50 younger people)
#'to determine if there was a relationship between age and memory.
#'10 Each subject was shown 27 words and asked to recall as many of those words as possible.
#'He also tested whether five different techniques impacted memory.
#'Each subject was given one of five types of instructions:
#' \describe{
#'   \item{Counting}{count the number of letters in each word}
#'   \item{Rhyming}{think of a word that rhymes with each word}
#'   \item{Adjective}{think of an adjective to describe each word}
#'   \item{Imagery}{create an image of each word}
#'   \item{Intentional}{remember as many words as possible}
#' }
#' The subjects in the first four groups were not aware that they would later be asked to recall each word.
#'
#' @format
#' A data frame with 100 rows and 4 columns:
#' \describe{
#'   \item{Age}{Young or old people}
#'   \item{Process}{Instruction types described anove}
#'   \item{Words}{Number of words remembered}
#'   \item{sqrtWords}{Square root of number of words remembered}
#' }
"MemoryA"


#' Towels2: Two-way Balanced Factorial Design
#'
#'Several television advertisements had claimed that a certain brand of paper towel was the strongest,
#'and these students wanted to determine if there really was a difference.
#'The students sampled 26 towels from two brands of paper towels, Comfort and Decorator.
#'Also, they were interested in whether the amount of water impacts the strength of either brand.
#'Weights (10, 25, 50, 100, or 250 grams) were slowly added to the center of each towel by a third person until it broke.
#' @format
#' A data frame with 156 rows and 4 columns:
#' \describe{
#'   \item{Brand}{Comfort or Decorator}
#'   \item{Water}{Drops of water}
#'   \item{Strength}{Weights in grams until paper towel breaks}
#'   \item{LogStrength}{Log of the weights in grams until paper towel breaks}
#' }
"Towels2"

#' Cups: Fractional Factorial Design
#'
#' A manager of a manufacturing company, shift managers, cup machine operators, and a statistician
#' decided to identify which factors were most influential in keeping their cups from leaking.
#' Over 30 possible factors were identified, but after some thoughtful discussions the group settled
#' on six variables that should be tested for their effects on leaking cups.
"Cups"


#' Tennis: Block Design
#'
#' Students on the college tennis team were interested in knowing if string tension
#' affected the speed and accuracy of a tennis ball.
#' After warming up, five men’s varsity tennis players volunteered to hit 30 serves using three different
#' racquet tensions over three days. Day was not treated as a factor of interest in the study,
#' but used to avoid fatigue in the players. For each serve, players aimed at a target in the back center
#' of the service box, and accuracy was measured as the distance in inches from the center of the
#' box to the ball strike location. A radar gun was used to measure the velocity of each player’s serve.
"Tennis"


#' Music: Block Design
#'
#' Students designed an experiment testing whether music tempo or length of test (1 minute or 3 minutes)
#' influenced students’ ability to type fast and accurately. Would subjects listening to Stayin’ Alive type
#' at a different speed than subjects listening to Yesterday by the Beatles?
#'
#' Forty undergraduate students consented to be in the study. Each subject took four tests from the website
#' typingtest.com in random order based on two coin flips: 1 min/Yesterday, 1 min/Stayin’ Alive,
#' 3 min/Yesterday, and 3 min/Stayin’ Alive. The questions the researcher wanted to test were the effect of Song,
#' Length, and Song*Length on words per minute (WPM). The Music dataset also records Accuracy and helps compute
#' average WPM for each subject (Subject.WPM.Ave), difference of the WPM of each trial and Subject.WPM.Ave,
#' and Accuracy*WPM (AWPM).
"Music"

#' Cookies: Split-Plot Design
#'
#' Two students wanted to determine if people could taste the difference in chocolate chip cookies
#' with varying amounts of sugar and varying amount of freshness. Nine batches were made, following the recipe
#' on the chocolate chip bag as closely as possible except for the amount of sugar.
#' Each batch was randomly assigned to one of three treatments: half the suggested amount of sugar,
#' the suggested amount of sugar, and double the suggested amount of sugar.
#'
#' On the day the nine batches of cookies
#' were baked, the researchers handed out five cookies from each batch (a total of 45 cookies)
#' to people in their dorm and asked them to rate the cookies. from 1 through 10, with 1 being inedible and 10
#' being the best cookie they every had. The research- ers stored the rest of the cookies for a day.
#' On the second day, the researchers handed out five more cookies from each of the original nine batches to
#' students in their dorm and asked them to rate them from 1 through 10. The researchers did the same thing
#' on the third day.
#'
#' Split-plot designs are often used when time is a second factor.
#' The whole-plot factor (Sugar) is randomly assigned to whole-plot units (Batch), and
#' then these same units (Batches) are measured at several time points (Day).
#' This is called a split plot in time, as the split plots are the time points within the units.
#'
#' Note that the factor Day is confounded with any other effect that occurs over time.
#' For example, suppose this study was conducted on a Saturday, Sunday, and Monday.
#' Students may have been more stressed on Monday and unknowingly tended to give lower scores on Monday.
#' Or more parents may have been around on the weekend and may have been more positive than students when
#' rating the cookies.
"Cookies"

#' Corn: Split-Plot Design
#'
#' Many seed companies and state-sponsored agricultural research centers have large testing areas
#' that have been shown to be similar to other plots of land throughout their state. In a large testing
#' area, eight 20-acre plots of land were randomly assigned to a nitrogen application rate (0, 70, 140,
#' or 210 pounds of nitrogen per acre). Each of these 20-acre plots was also subdivided into five
#' 4-acre subplots. Within each 20-acre plot, the subplots were assigned to be planted with one species
#' of hybrid corn (A, B, C, D, or E). At the end of the season, the fields were harvested and the yield
#' in bushels per acre was recorded.
"Corn"

#' Cookies2: Block Design
#'
#' Two students wanted to test whether ingredients (butter, Fleischmann’s corn oil margarine,
#' or unflavored Crisco), cooking time (short or long) or cookie type (chocolate chip or gingersnap)
#' influenced taste ratings. Both main effects and interactions were of interest.
#' Twelve volunteers were found who were each willing to taste
#' 12 cookies in random order (one taste for each of the two cookie types,
#' the three ingredient types, and the two cooking times).
#' Each volunteer ranked all 12 cookies on a scale from 1 to 10 (10 being the best).
"Cookies2"

#' Football: Split-Plot Design
#'
#'Ben, Hugh, and Alex wanted to determine if the size of the “football” made a
#'difference in scoring accuracy in the game of paper football. (Details of the simple
#'tabletop game can be found at http:// www.paperfootball.us.) In addition, these students
#'were interested in knowing if the effect of football size was dependent on a player’s experience level.
#'
#'The researchers set up a goal that was 8 inches above a table and 10 inches wide.
#'The subjects kicked/flicked the football from 42 inches away from the goal.
#'
#'There were 18 volunteers, who self-identified as experienced or inexperienced players.
#'After a few practice kicks, each subject kicked 20 small and 20 large footballs in random order
#'(flipping a coin before each kick) at the goal. The response for this study was the proportion
#'of successful goals.
"Football"


#' Colors: Split-Plot Design
#'
#' Kastner et al. studied how the brain recognizes the shape and color of an object. They found that the
#' process of identifying the shape and color of an item is carried out not simultaneously but in steps.
#'
#' Two students decided to investigate the impact of color distraction on a shape matching game
#' called Shapesplosion. In this game, subjects are asked to match shapes by placing specifically
#' shaped pegs into the matching hole as quickly as possible. These student researchers selected
#' eight female science majors and eight female non-science majors at their college.
#' Each of the 16 subjects played four games (each of the following four games in random order):
#'
#' Factor 1 (game complexity): the Shapesplosion game with 15 pieces (pegs) or the game with 18 pieces
#'
#' Factor 2 (color distracter): the Shapesplosion game where the peg color matches the hole
#' color or the game where the pegs are a different color than the hole
#'
#' The Time it took (in seconds) to complete each game was recorded.
"Colors"


#' Colors2: Split-Plot Design
#'
#' In addition to the 16 females tested in the 'Colors' dataset, 16 males were also tested in this dataset.
"Colors2"


#' Flower: Block Design
#'
#' Students in an introductory statistics class tested the impact of different types of
#' water solutions on the lon- gevity of cut flowers. They purchased 18 white carnations from three different
#' randomly selected  stores and randomly assigned each flower to one of three treatments
#' (plain water, one aspirin crushed and added
#' to the water, and a floral compound provided by the flower shop) and then measured how many days
#' it took until the flower wilted.
"Flower"

#' C5Popcorn: Block Design
#'
#' Two students in a design of experiments course wanted to test if the price and the storage location
#'  of popcorn influenced the percentage of kernels that popped. The students purchased three boxes
#'  of both an expensive and a generic popcorn brand (labeled Exp and Gen). Each box contained six
#'  microwavable bags of popcorn. Two bags were randomly selected from each box and stored for one week,
#'  one in the refrigerator (Frig) and the other at room temperature (Room).
#'  The bags were popped in random order according to the instructions on the box, and
#'  the percentage of popped kernels was calculated for each bag.
"C5Popcorn"

#' Handwash: Block Design
#'
#' Two students are interested in the effectiveness of three types of cleansers. They also sort CFU counts
#'  into three blocks (representing low, medium, and high CFU counts).
"Handwash"

#' Memory: Split-Plot Design
#'
#' Two students in a statistics course, Josh and Ann, were interested in conducting an experiment
#' to help them better understand memory. For each test, Josh and Ann decided to test what psychologists
#' call free recall by asking each subject to read through a list of 20 words for 30 seconds,
#' work through a distracter, and then recall as many words as he or she could.
#' Josh and Ann asked each subject to take all four tests in random order, with a short break between tests.
#'
#' In this experiment, each unit is a test that is randomly assigned to one of the four factor-level combinations listed below:
#'
#' 1. Abstract word list and mathematics distracter
#'
#' 2. Abstract word list and poetry distracter
#'
#' 3. Concrete word list and mathematics distracter
#'
#' 4. Concrete word list and poetry distracter
#'
#' Also, students are randomly selected from four different majors.
"Memory"
