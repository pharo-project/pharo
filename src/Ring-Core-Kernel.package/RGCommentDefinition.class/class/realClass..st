instance creation
realClass: aClass
	"Creates a ring comment definition from a Smalltalk class"
	^(super realClass: aClass)
		asActive;
		yourself