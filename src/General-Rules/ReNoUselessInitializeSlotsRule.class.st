"
Some Pharo's slots needs initialization and are sending #initializeSlots: in the initialization method of their classes. But it happens that the slots get removed and the #initializeSlots: method call does not get removed. This causes performance issues on objects instatiated a lot.
"
Class {
	#name : 'ReNoUselessInitializeSlotsRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'testing' }
ReNoUselessInitializeSlotsRule class >> checksClass [

	^ true
]

{ #category : 'accessing' }
ReNoUselessInitializeSlotsRule class >> groupSelector [

	^ #optimizationGroup
]

{ #category : 'accessing' }
ReNoUselessInitializeSlotsRule class >> ruleName [

	^ 'Unused #initializeSlots: call'
]

{ #category : 'running' }
ReNoUselessInitializeSlotsRule >> check: aClass forCritiquesDo: aCriticBlock [

	aClass compiledMethodAt: #initialize ifPresent: [ :initialize | (initialize refersToLiteral: #initializeSlots:) ifFalse: [ ^ self ] ] ifAbsent: [ ^ self ].

	(aClass allSlots anySatisfy: [ :slot | slot wantsInitialization ]) ifTrue: [ ^ self ].

	(aClass hasSubclasses and: [ Class subclasses allSatisfy: [ :subclass | subclass slots anySatisfy: [ :slot | slot wantsInitialization ] ] ]) ifTrue: [ ^ self ].

	aCriticBlock cull: (self critiqueFor: aClass about: aClass name)
]
