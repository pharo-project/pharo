instance creation
implementorsOf: aSelector in: anEnvironment 
	| classDict metaDict selectors |
	classDict := IdentityDictionary new.
	metaDict := IdentityDictionary new.
	selectors := IdentitySet with: aSelector.
	anEnvironment classesDo: [ :class | 
		((class includesLocalSelector: aSelector) and: [ anEnvironment includesSelector: aSelector in: class ]) 
			ifTrue: [
				class isMeta 
					ifTrue: [ metaDict at: class soleInstance name put: selectors copy ]
					ifFalse: [ classDict at: class name put: selectors copy ] ] ].
	^ (self onEnvironment: anEnvironment)
		classSelectors: classDict metaClassSelectors: metaDict;
		label: 'Implementors of ' , aSelector;
		yourself