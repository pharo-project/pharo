"
I am the combination of two RBEnvironments, a logical OR. That is: 
entity A is in this environment if it is in at least ONE environment I am constructed from.

Do not construct instances of me directly, use method #| for two existing environments:
env1 | env2 -> a RBOrEnvironment.
"
Class {
	#name : #RBOrEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		'orEnvironment'
	],
	#category : #'Refactoring-Environment'
}

{ #category : #'instance creation' }
RBOrEnvironment class >> onEnvironment: anEnvironment or: anotherEnvironment [
	^ (self onEnvironment: anEnvironment)
		orEnvironment: anotherEnvironment;
		yourself
]

{ #category : #accessing }
RBOrEnvironment >> classesDo: aBlock [
	| enumerated |
	enumerated := IdentitySet new.
	environment classesDo: [ :each | 
		aBlock value: each.
		enumerated add: each].
	orEnvironment classesDo: [ :each | 
		(enumerated includes: each) 
			ifFalse: [ aBlock value: each ] ]
]

{ #category : #testing }
RBOrEnvironment >> definesClass: aClass [
	^ (environment definesClass: aClass) or: [ 
		orEnvironment definesClass: aClass ]
]

{ #category : #testing }
RBOrEnvironment >> includesCategory: aCategory [ 
	^ (environment includesCategory: aCategory)
		or: [ orEnvironment includesCategory: aCategory ]
]

{ #category : #testing }
RBOrEnvironment >> includesClass: aClass [
	(environment includesClass: aClass) ifTrue: [ ^ true ].
	(orEnvironment includesClass: aClass) ifTrue: [ ^ true ].
	(environment selectorsForClass: aClass) ifNotEmpty: [ ^ true ].
	(orEnvironment selectorsForClass: aClass) ifNotEmpty: [ ^ true ].
	^ false
]

{ #category : #testing }
RBOrEnvironment >> includesProtocol: aProtocol in: aClass [
	^ (environment includesProtocol: aProtocol in: aClass)
		or: [ orEnvironment includesProtocol: aProtocol in: aClass ]
]

{ #category : #testing }
RBOrEnvironment >> includesSelector: aSelector in: aClass [
	^ (environment includesSelector: aSelector in: aClass)
		or: [ orEnvironment includesSelector: aSelector in: aClass ]
]

{ #category : #initialization }
RBOrEnvironment >> orEnvironment: aBrowserEnvironment [ 
	orEnvironment := aBrowserEnvironment
]

{ #category : #accessing }
RBOrEnvironment >> packages [
	^ environment packages | orEnvironment packages
]

{ #category : #accessing }
RBOrEnvironment >> selectionIntervalFor: aString [
	^ (environment selectionIntervalFor: aString)
		ifNil: [ orEnvironment selectionIntervalFor: aString ]
]

{ #category : #accessing }
RBOrEnvironment >> selectorsForClass: aClass do: aBlock [
	| enumerated |
	enumerated := IdentitySet new.
	environment
		selectorsForClass: aClass
		do: [ :each | 
			enumerated add: each.
			aBlock value: each ].
	orEnvironment
		selectorsForClass: aClass
		do: [:each | 
			(enumerated includes: each) 
				ifFalse: [ aBlock value: each ] ]
]

{ #category : #printing }
RBOrEnvironment >> storeOn: aStream [ 
	aStream nextPut: $(.
	environment storeOn: aStream.
	aStream nextPutAll: ' | '.
	orEnvironment storeOn: aStream.
	aStream nextPut: $)
]
