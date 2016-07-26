accessing
enabled: aBoolean
	aBoolean ~= enabled
		ifTrue: [ 
			enabled := aBoolean.
			self  requestView: MorphTreeChangeRequest enabled]