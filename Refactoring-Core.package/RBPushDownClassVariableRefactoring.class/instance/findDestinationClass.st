preconditions
findDestinationClass
	| classes |
	classes := class withAllSubclasses reject: [ :each |
		(each whichSelectorsReferToClassVariable: variableName) isEmpty
			and: [ (each theMetaClass whichSelectorsReferToClassVariable: variableName) isEmpty ] ].
	destinationClass := classes isEmpty 
		ifTrue: [ nil ]
		ifFalse: [ classes asOrderedCollection first ].
	classes do: [ :each | 
		(destinationClass includesClass: each) 
			ifTrue: [ destinationClass := each ]
				ifFalse: [
					(each includesClass: destinationClass) 
						ifFalse: [ self signalMultipleReferenceError ] ] ].
	destinationClass = class 
		ifTrue: [ self signalStillReferencedError ].
	^ destinationClass