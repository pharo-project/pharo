utilities
confirm: queryString
	"Put up a yes/no menu with caption queryString. Answer true if the response is yes, false if no. This is a modal question--the user must respond yes or no. Do not invoke this method - it is here to help in case of mini system, better use
	UIManager default confirm: 'Is is the question?' "

	^ self confirm: queryString trueChoice: 'Yes' translated falseChoice: 'No' translated