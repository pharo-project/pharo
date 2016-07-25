accessing-classes
allClasses
	"Answer all the unique non-metaclasses of all the classes and metaclasses in this environment."

	| allClasses |
	allClasses := IdentitySet new: 4096.
	self classesDo: [ :each | 
		allClasses add: each theNonMetaClass].
	^ allClasses asArray