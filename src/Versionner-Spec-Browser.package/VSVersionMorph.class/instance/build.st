private
build	
	version isStable 
		ifTrue: [ self addTag: #stable ].
	version isDevelopment 
		ifTrue: [ self addTag: #development ].	
	self addAllMorphs: tags.
	version isDevelopment
		ifFalse: [  self addMorphFront: version name asMorph ]