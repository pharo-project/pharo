services
chooseDropList: aStringOrText title: aString list: aList
	"Open a drop list chooser dialog."

	^self theme
		chooseDropListIn: self
		text: aStringOrText
		title: aString
		list: aList