actions api
after: afterBlock do: doBlock ifTrue: conditionBlock
	|value|
	value := conditionBlock brickValue: self.
	afterBlock brickValue: self.
	value ifTrue: [ doBlock brickValue: value ]