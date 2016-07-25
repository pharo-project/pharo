query
allExistingProtocolsFor: instanceSide

	"answer all protocols on instance or class side"
	
	| allExistingProtocols |
	allExistingProtocols := Smalltalk allClassesAndTraits
		inject: IdentitySet new
		into: [ :col :e | 
			| class |
			class := instanceSide
				ifTrue: [ e theNonMetaClass ]
				ifFalse: [ e theMetaClass ].
			col
				addAll: class protocols;
				yourself ].
	^ allExistingProtocols