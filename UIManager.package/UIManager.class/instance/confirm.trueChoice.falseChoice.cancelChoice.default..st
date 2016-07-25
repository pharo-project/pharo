ui requests
confirm: queryString trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: aSymbol
	"Put up a yes/no/cancel menu with caption queryString. The actual wording for the choices will be as provided in the trueChoice, falseChoice and cancelChoice parameters.
	Default should be one of #yes, #no or #cancel to set the default button.
	Answer true if the response is the true-choice, false if it's the false-choice, nil if the cancelChoice.
	This is a modal question -- the user must respond."
	
	^self subclassResponsibility