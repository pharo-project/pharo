transforming
transformBy: aDisplayTransform during: aBlock
	| myTransform result |
	myTransform := transform.
	self transformBy: aDisplayTransform.
	result := aBlock value: self.
	transform := myTransform.
	^result