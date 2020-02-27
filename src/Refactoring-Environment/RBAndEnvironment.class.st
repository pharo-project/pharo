"
I am the combination of two RBEnvironments, a logical AND. That is: 
entity A is in this environment if it is in BOTH environment I am constructed from.

Do not construct instances of me directly, use method #& for two existing environments:
env1 & env2 -> a RBAndEnvironment.
"
Class {
	#name : #RBAndEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		'andedEnvironment'
	],
	#category : #'Refactoring-Environment'
}

{ #category : #'instance creation' }
RBAndEnvironment class >> onEnvironment: anEnvironment and: anotherEnvironment [ 
	^(self onEnvironment: anEnvironment)
		andedEnvironment: anotherEnvironment;
		yourself
]

{ #category : #private }
RBAndEnvironment >> andedEnvironment [
	^andedEnvironment
]

{ #category : #initialization }
RBAndEnvironment >> andedEnvironment: aBrowserEnvironment [ 
	andedEnvironment := aBrowserEnvironment
]

{ #category : #testing }
RBAndEnvironment >> classes [
	^ environment classes & andedEnvironment classes
]

{ #category : #accessing }
RBAndEnvironment >> classesDo: aBlock [ 
	environment 
		classesDo: [:each | (self includesClass: each) ifTrue: [aBlock value: each]]
]

{ #category : #testing }
RBAndEnvironment >> definesClass: aClass [
	^ (environment definesClass: aClass) and: [ 
		andedEnvironment definesClass: aClass ]
]

{ #category : #testing }
RBAndEnvironment >> includesCategory: aCategory [ 
	^(self classNamesFor: aCategory) isNotEmpty
]

{ #category : #testing }
RBAndEnvironment >> includesClass: aClass [ 
	| doesntHaveSelectors |
	(environment includesClass: aClass) ifFalse: [^false].
	(andedEnvironment includesClass: aClass) ifFalse: [^false].
	doesntHaveSelectors := true.
	environment selectorsForClass: aClass
		do: 
			[:each | 
			doesntHaveSelectors := false.
			(andedEnvironment includesSelector: each in: aClass) ifTrue: [^true]].
	^doesntHaveSelectors
]

{ #category : #testing }
RBAndEnvironment >> includesProtocol: aProtocol in: aClass [ 
	^ (environment includesProtocol: aProtocol in: aClass)
		and: [ andedEnvironment includesProtocol: aProtocol in: aClass ]
]

{ #category : #testing }
RBAndEnvironment >> includesSelector: aSelector in: aClass [ 
	^(environment includesSelector: aSelector in: aClass) 
		and: [andedEnvironment includesSelector: aSelector in: aClass]
]

{ #category : #accessing }
RBAndEnvironment >> packages [
	^ environment packages & andedEnvironment packages
]

{ #category : #printing }
RBAndEnvironment >> printOn: aStream [ 
	environment printOn: aStream.
	aStream nextPutAll: ' & '.
	andedEnvironment printOn: aStream.
]

{ #category : #accessing }
RBAndEnvironment >> problemCount [
	^environment isClassEnvironment 
		ifTrue: [self numberClasses]
		ifFalse: [super problemCount]
]

{ #category : #accessing }
RBAndEnvironment >> selectionIntervalFor: aString [ 
	| interval |
	interval := super selectionIntervalFor: aString.
	interval ifNotNil: [^interval].
	^andedEnvironment selectionIntervalFor: aString

]

{ #category : #accessing }
RBAndEnvironment >> selectorsForClass: aClass do: aBlock [ 
	environment selectorsForClass: aClass
		do: 
			[:each | 
			(andedEnvironment includesSelector: each in: aClass) 
				ifTrue: [aBlock value: each]]
]

{ #category : #printing }
RBAndEnvironment >> storeOn: aStream [ 
	aStream nextPut: $(.
	environment storeOn: aStream.
	aStream nextPutAll: ' & '.
	andedEnvironment storeOn: aStream.
	aStream nextPut: $)
]
