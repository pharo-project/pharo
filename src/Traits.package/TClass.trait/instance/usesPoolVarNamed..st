pool variables
usesPoolVarNamed: aString
	"Return whether the receiver has a pool variable named: aString, taking into account superclasses too"
	
	self allSharedPools do: [:each | (each usesClassVarNamed: aString) ifTrue: [^true]].
	^false