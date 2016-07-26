drawing-private
computeSubmorphsToDraw

	owner width < tab showIconTreshold 
		ifTrue: [ 
			(storedIcon isNil and: [ icon notNil ]) ifTrue: [
				storedIcon := icon.
				self removeMorph: icon ].
			^ { morph } ].
		
	storedIcon 
		ifNotNil: [ 
			self addMorphFront: storedIcon.
			storedIcon := nil ].
		
	^ self submorphs