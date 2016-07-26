ui requests
confirm: queryString trueChoice: trueChoice falseChoice: falseChoice 
	"Put up a yes/no menu with caption queryString. The actual wording for the two choices will be as provided in the trueChoice and falseChoice parameters. Answer true if the response is the true-choice, false if it's the false-choice.
	This is a modal question -- the user must respond one way or the other."
	^self subclassResponsibility