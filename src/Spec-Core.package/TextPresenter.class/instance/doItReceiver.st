api-doIt
doItReceiver
	| receiver |
	
	receiver := doItReceiver value.
	receiver isBehavior ifTrue: [receiver  := receiver theNonMetaClass].
	^receiver