instance creation
new
	self name == #MetacelloMCProjectSpec
		ifTrue: [ self error: 'This class is abstract' ].
	^ super new