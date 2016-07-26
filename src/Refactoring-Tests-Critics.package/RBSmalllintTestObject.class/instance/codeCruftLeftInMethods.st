methods
codeCruftLeftInMethods
	| a b c |
	a := b := c := 2.
	self halt.
	^ a > 0
		ifTrue: [ b ]
		ifFalse: [ c ]
		