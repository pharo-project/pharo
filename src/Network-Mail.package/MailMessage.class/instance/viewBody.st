printing/formatting
viewBody
	"open a viewer on the body of this message"
	self containsViewableImage
		ifTrue: [^ self viewImageInBody].
	UIManager default  
		longMessage: self bodyTextFormatted
		title: (self name ifNil: ['(a message part)'])
