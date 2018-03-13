initializing
lookup: anInstanceVariable in: aClass
	| slot |
	
	"I lookup the indices in the RGEnvironment, if one of the classes does not exists (like Traits) I put nil, I should not explode. 
	If it explode we have a problem."
	slot := (environment ask behaviorNamed: aClass ifAbsent: [ ^nil ]) allSlots
		detect: [ :x | x name = anInstanceVariable ]
		ifNone: [ self error: 'Cannot find:' , anInstanceVariable , ' in ' , aClass ].
	^ slot index