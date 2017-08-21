tests
testReferencedClasses
	
	| refs |
	refs := Metaclass referencedClasses.
	self assert: (refs includes: SystemAnnouncer).
	
	refs := self class referencedClasses.
	self assert: (refs includesAll: { SystemAnnouncer. Metaclass })