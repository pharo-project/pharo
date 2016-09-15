private - utilities
confirm: queryString trueChoice: trueChoice falseChoice: falseChoice
	"Put up a yes/no menu with caption queryString. The actual wording for the two choices will be as provided in the trueChoice and falseChoice parameters. Answer true if the response is the true-choice,  false if it's the false-choice. This is a modal question -- the user must respond one way or the other.  Do not invoke this method - it is here to help in case of mini system, better use UIManager default"

	"MenuMorph 
		confirm: 'Are you sure?'  
		trueChoice: 'yes, I''m '  
		falseChoice: 'no, I just thought'"

	| menu aBlock result |
	(ProvideAnswerNotification signal: queryString)
		ifNotNil: [ :answer | ^ trueChoice = answer ].
	aBlock := [ :v | result := v ].
	menu := self new.
	menu addTitle: queryString icon: (self iconNamed: #confirmIcon).
	menu
		add: trueChoice
		target: aBlock
		selector: #value:
		argument: true.
	menu
		add: falseChoice
		target: aBlock
		selector: #value:
		argument: false.
	[ menu
		invokeAt: ActiveHand position
		in: ActiveWorld
		allowKeyboard: true.
	result isNil ] whileTrue.
	^ result