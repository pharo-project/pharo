instance creation
newFor: class
	^ self new
		methodClass: class;
		yourself