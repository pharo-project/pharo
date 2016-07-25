accessing-classes
classFor: aBehavior 
	aBehavior isNil 
		ifTrue: [ ^ nil ].
	^ aBehavior isMeta 
		ifTrue: [ self metaclassNamed: aBehavior theNonMetaClass name ]
		ifFalse: [ self classNamed: aBehavior theNonMetaClass name ]