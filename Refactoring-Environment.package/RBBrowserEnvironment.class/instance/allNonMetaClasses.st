accessing-classes
allNonMetaClasses
	"Answer all the unique non-metaclasses of all of all the non-metaclasses in this environment."
	| allNonMetaClasses |
	allNonMetaClasses := OrderedCollection new: 4096.
	self
		classesDo: [ :each | 
			each isMeta
				ifFalse: [ allNonMetaClasses add: each  ] ].
	^ allNonMetaClasses asArray