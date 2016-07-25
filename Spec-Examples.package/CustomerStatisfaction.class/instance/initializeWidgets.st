initialization
initializeWidgets
	"widgets instanciation"

	screen := self instantiate: LabelModel.
	buttonHappy := self instantiate: ButtonModel.
	buttonNeutral := self instantiate: ButtonModel.
	buttonBad := self instantiate: ButtonModel.

	"widgets configuration specification"
	screen label: 'Tell us ?'.
	buttonHappy
		label: 'Happy';
		icon: (self iconNamed: #thumbsUp).
	buttonNeutral
		label: 'Neutral';
		icon: (self iconNamed: #user).
	buttonBad
		label: 'Bad';
		icon: (self iconNamed: #thumbsDown).

	"specification of order of focus"
	self focusOrder
		add: buttonHappy;
		add: buttonNeutral;
		add: buttonBad