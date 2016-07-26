Fraction provides methods for dealing with fractions like 1/3 as fractions (not as 0.33333...).  All public arithmetic operations answer reduced fractions (see examples).

instance variables: 'numerator denominator '

Examples: (note the parentheses required to get the right answers in Smalltalk and Pharo):

(2/3) + (2/3)
(2/3) + (1/2)		 "answers shows the reduced fraction" 
(2/3) raisedToInteger: 5		 "fractions also can have exponents"
