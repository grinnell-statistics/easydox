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
