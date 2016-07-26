accessing-classes
allMetaClasses
	"Answer all the unique non-metaclasses of all metaclasses in this environment."

	| allMetaClasses |
	allMetaClasses := OrderedCollection new: 4096.
	self
		classesDo: [ :each | 
			each isMeta
				ifTrue: [ allMetaClasses add: each theNonMetaClass ] ].
	^ allMetaClasses asArray