private
orientation
	| dir |
	dir := layoutProperties listDirection.
	^ ((dir == #topToBottom) | (dir == #bottomToTop))
		ifTrue: [ #vertical ]
		ifFalse: [ #horizontal ]