brick-interactable-actions
select
	self owner ifNil: [ ^ self ].
	self minWidth: [ self owner width min: (self hasSubbricks
		ifTrue: [ (self label layout maxWidth brickValue: self label) ]
		ifFalse: [ 0 ]) ].
	self owner doLayoutForce