geometry
width
	^  self wrapped
		ifTrue: [ self container right  -  (self container insetBy: self margins) topLeft x]
		ifFalse: [ SmallInteger maxVal ]