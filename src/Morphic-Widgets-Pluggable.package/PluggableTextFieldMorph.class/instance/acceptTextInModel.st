model access
acceptTextInModel
	"Inform the model that the receiver's textMorph's
	text should be accepted.
	Answer true if the model accepted ok, false
	otherwise"
	| objectToAccept text |
	self hasValidText ifFalse: [^false].
	text := self text.
	objectToAccept := self converter
				ifNil: [text]
				ifNotNil: [self default
						ifNil: [self converter stringAsObject: text asString]
						ifNotNil: [text
								ifEmpty: [self default]
								ifNotEmpty: [self converter stringAsObject: text asString]]].
	^ setTextSelector isNil
		or: [setTextSelector numArgs = 2
				ifTrue: [model
						perform: setTextSelector
						with: objectToAccept
						with: self]
				ifFalse: [model perform: setTextSelector with: objectToAccept]]