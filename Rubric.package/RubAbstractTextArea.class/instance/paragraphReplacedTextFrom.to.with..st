paragraph feedbacks
paragraphReplacedTextFrom: start to: stop with: aText
	"A text change has taken place in my paragraph, as a result of editing and I must be updated"
	self
		handleBoundsChange: [ 
			self wrapped
				ifTrue: [ self updateBottomFromParagraph ]
				ifFalse: [ self updateExtentFromParagraph ] ].
	self scrollPane ifNotNil: [ :sp | sp textChanged ].
	self announce: (RubTextChanged from: start to: stop with: aText).
