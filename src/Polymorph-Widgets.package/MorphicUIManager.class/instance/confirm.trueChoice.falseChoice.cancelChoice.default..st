services
confirm: queryString trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: defaultOption
	"Put up a yes/no/cancel menu with caption queryString. The actual wording for the choices will be as provided in the trueChoice, falseChoice and cancelChoice parameters.
	defaultOption should be one of true, false or nil to set the default button.
	Answer true if the response is the true-choice, false if it's the false-choice, nil if the cancelChoice.
	This is a modal question -- the user must respond."

	^ self
		confirm: queryString
		label: 'Question' translated
		trueChoice: trueChoice
		falseChoice: falseChoice
		cancelChoice: cancelChoice
		default: defaultOption