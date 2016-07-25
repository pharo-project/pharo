instance creation
implementorsMatching: aString in: anEnvironment 
	| classDict metaDict |
	classDict := IdentityDictionary new.
	metaDict := IdentityDictionary new.
	anEnvironment classesDo: [ :class | 
		| selectors |
		selectors := IdentitySet new.
		anEnvironment selectorsForClass: class
			do: [ :each | (aString match: each) ifTrue: [ selectors add: each ] ].
		selectors isEmpty ifFalse: [
			class isMeta 
				ifTrue: [ metaDict at: class soleInstance name put: selectors ]
				ifFalse: [ classDict at: class name put: selectors ] ] ].
	^ (self onEnvironment: anEnvironment)
		classSelectors: classDict metaClassSelectors: metaDict;
		label: 'Implementors of ' , aString;
		yourself