accessing
text: aTextOrString
	text := aTextOrString.
	self glamourPresentation ifNotNil: [ :presentation | presentation text: text ]