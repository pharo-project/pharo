*Morphic-Base
assureTableProperties
	| props |
	props := self layoutProperties.
	props == self ifTrue:[props := nil].
	props ifNil:[
		props := TableLayoutProperties new initializeFrom: self.
		self layoutProperties: props].
	props includesTableProperties 
		ifFalse:[self layoutProperties: (props := props asTableLayoutProperties)].
	^props