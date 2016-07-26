services
chooseDropList: aStringOrText list: aList
	"Open a drop list chooser dialog."

	^self
		chooseDropList: aStringOrText
		title: 'Choose' translated
		list: aList