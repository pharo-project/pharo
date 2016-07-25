*Morphic-Base
assureLayoutProperties
	| props |
	props := self layoutProperties.
	props == self ifTrue:[props := nil].
	props ifNil:[
		props := LayoutProperties new initializeFrom: self.
		self layoutProperties: props].
	^props