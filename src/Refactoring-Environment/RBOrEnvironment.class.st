"
I am the combination of two RBEnvironments, a logical OR. That is: 
entity A is in this environment if it is in at least ONE environment I am constructed from.

Do not construct instances of me directly, use method #| for two existing environments:
env1 | env2 -> a RBOrEnvironment.
"
Class {
	#name : 'RBOrEnvironment',
	#superclass : 'RBCompositeEnvironment',
	#category : 'Refactoring-Environment',
	#package : 'Refactoring-Environment'
}

{ #category : 'instance creation' }
RBOrEnvironment class >> onEnvironment: anEnvironment or: anotherEnvironment [
	^ (self onEnvironment: anEnvironment)
		orEnvironment: anotherEnvironment;
		yourself
]

{ #category : 'accessing' }
RBOrEnvironment >> classesDo: aBlock [
	| enumerated |
	enumerated := IdentitySet new.
	environment classesDo: [ :each |
		aBlock value: each.
		enumerated add: each].
	self orEnvironment classesDo: [ :each |
		(enumerated includes: each)
			ifFalse: [ aBlock value: each ] ]
]

{ #category : 'testing' }
RBOrEnvironment >> definesClass: aClass [
	^ (environment definesClass: aClass) or: [
		self orEnvironment definesClass: aClass ]
]

{ #category : 'testing' }
RBOrEnvironment >> includesClass: aClass [
	(environment includesClass: aClass) ifTrue: [ ^ true ].
	(self orEnvironment includesClass: aClass) ifTrue: [ ^ true ].
	(environment selectorsForClass: aClass) ifNotEmpty: [ ^ true ].
	(self orEnvironment selectorsForClass: aClass) ifNotEmpty: [ ^ true ].
	^ false
]

{ #category : 'testing' }
RBOrEnvironment >> includesProtocol: aProtocol in: aClass [

	^ (environment includesProtocol: aProtocol in: aClass) or: [ self orEnvironment includesProtocol: aProtocol in: aClass ]
]

{ #category : 'testing' }
RBOrEnvironment >> includesSelector: aSelector in: aClass [
	^ (environment includesSelector: aSelector in: aClass)
		or: [ self orEnvironment includesSelector: aSelector in: aClass ]
]

{ #category : 'accessing' }
RBOrEnvironment >> methodsForClass: aClass do: aBlock [

	| enumerated |
	enumerated := IdentitySet new.
	environment methodsForClass: aClass do: [ :method |
		enumerated add: method.
		aBlock value: method ].
	self orEnvironment methodsForClass: aClass do: [ :method | (enumerated includes: method) ifFalse: [ aBlock value: method ] ]
]

{ #category : 'accessing' }
RBOrEnvironment >> operator [
	^ ' | '
]

{ #category : 'accessing' }
RBOrEnvironment >> orEnvironment [
	^ otherEnvironment
]

{ #category : 'accessing' }
RBOrEnvironment >> orEnvironment: aBrowserEnvironment [
	 otherEnvironment := aBrowserEnvironment
]

{ #category : 'accessing' }
RBOrEnvironment >> selectionIntervalFor: aString [
	^ (environment selectionIntervalFor: aString)
		ifNil: [ self orEnvironment selectionIntervalFor: aString ]
]

{ #category : 'accessing' }
RBOrEnvironment >> selectorsForClass: aClass do: aBlock [
	| enumerated |
	enumerated := IdentitySet new.
	environment
		selectorsForClass: aClass
		do: [ :each |
			enumerated add: each.
			aBlock value: each ].
	self orEnvironment
		selectorsForClass: aClass
		do: [:each |
			(enumerated includes: each)
				ifFalse: [ aBlock value: each ] ]
]

{ #category : 'printing' }
RBOrEnvironment >> storeOn: aStream [
	aStream nextPut: $(.
	environment storeOn: aStream.
	aStream nextPutAll: self operator.
	self orEnvironment storeOn: aStream.
	aStream nextPut: $)
]
