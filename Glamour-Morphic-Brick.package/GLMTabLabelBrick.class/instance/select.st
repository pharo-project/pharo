brick-interactable-actions
select
	self owner ifNil: [ ^ self ].
	self minWidth: [ (self label layout maxWidth brickValue: self label) min: self owner width ].
	self owner doLayoutForce