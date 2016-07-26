accessing
checkRectangle
	| tr |
	tr := self toggleRectangle translateBy: (3 @ 0).
	^ self mustTakeIntoAccountToggleSpace 
		ifTrue: [(tr topRight + (self checkGap @ 0)) corner:  tr bottomRight + ((self checkGap + self checkWidth) @ 0)]
		ifFalse: [tr]
