Extension { #name : #Slot }

{ #category : #'*GT-SpotterExtensions-Core' }
Slot >> spotterAccessesFor: aStep [
	<spotterOrder: 10>
	(aStep hasPreviousStep and: [
		(aStep previousStep origin isKindOf: Class) or: [
			aStep origin isKindOf: Class]]) ifTrue: [
		aStep listProcessor
				title: 'Accesses';
				allCandidates: [  
						(aStep previousStep origin slotNamed: self name) usingMethods ];
				itemName: [:each | each asString];
				filter: GTFilterSubstring ]
]
