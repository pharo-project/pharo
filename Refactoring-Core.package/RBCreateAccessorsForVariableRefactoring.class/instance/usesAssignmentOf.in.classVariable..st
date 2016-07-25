testing
usesAssignmentOf: aString in: aClass classVariable: isClassVar 
	| matcher definingClass |
	matcher := RBParseTreeSearcher new.
	matcher
		answer: false;
		matches: aString , ' := ``@object'
			do: [ :aNode :answer | answer or: [ aNode isUsed ] ].
	definingClass := isClassVar 
		ifTrue: [ aClass theNonMetaClass ]
		ifFalse: [ aClass ].
	^ (definingClass withAllSubclasses , (isClassVar 
			ifTrue: [ definingClass theMetaClass withAllSubclasses ]
			ifFalse: 
				[ #() ]) 
		detect: 
			[ :each | 
			((isClassVar 
				ifTrue: [ each whichSelectorsReferToClassVariable: aString ]
				ifFalse: [ each whichSelectorsReferToInstanceVariable: aString ]) 
				detect: 
					[ :sel | 
					self 
						checkClass: each
						selector: sel
						using: matcher ]
				ifNone: [ nil ]) notNil ]
		ifNone: [ nil ]) notNil