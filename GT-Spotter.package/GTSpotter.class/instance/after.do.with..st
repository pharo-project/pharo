actions api
after: afterBlock do: doBlock with: withBlock
	|value|
	value := withBlock brickValue: self.
	afterBlock brickValue: self.
	doBlock brickValue: value