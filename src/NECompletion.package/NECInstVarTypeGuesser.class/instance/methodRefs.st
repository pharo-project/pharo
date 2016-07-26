public
methodRefs
	| theClass selectors |
	theClass := receiverClass classThatDefinesInstanceVariable: variableName.
	theClass ifNil: [ ^ nil ].
	selectors := theClass whichSelectorsStoreInto: variableName.
	^ selectors collect: [ :each | 
		RGMethodDefinition realClass: theClass selector: each]