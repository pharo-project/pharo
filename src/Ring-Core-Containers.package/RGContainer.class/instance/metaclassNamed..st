lookup by name
metaclassNamed: metaclassName
	| theClass |
	
	theClass:= (self classOrTraitNamed: metaclassName).
	theClass notNil
	ifTrue:[ ^theClass isMeta 
		      ifTrue: [ theClass ]
		      ifFalse:[ theClass theMetaClass ] ].
	
	^nil