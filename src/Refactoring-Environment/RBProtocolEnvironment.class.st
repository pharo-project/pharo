"
I am a RBBrowserEnvironment on a set of protocols of a class.

Example:
(RBBrowserEnvironment new) forClass:Morph protocols:{'printing'}.
"
Class {
	#name : #RBProtocolEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		'class',
		'protocols'
	],
	#category : #'Refactoring-Environment'
}

{ #category : #'instance creation' }
RBProtocolEnvironment class >> class: aClass protocols: aCollection [
	^ self 
		onEnvironment: self default
		class: aClass
		protocols: aCollection
]

{ #category : #'instance creation' }
RBProtocolEnvironment class >> onEnvironment: anEnvironment class: aClass protocols: aCollection [
	^(self onEnvironment: anEnvironment)
		class: aClass protocols: aCollection; yourself
]

{ #category : #initialization }
RBProtocolEnvironment >> class: aClass protocols: aCollection [
	class := aClass.
	protocols := aCollection
]

{ #category : #private }
RBProtocolEnvironment >> defaultLabel [
	| stream |
	stream := String new writeStream.
	stream nextPutAll: class name;
		nextPut: $>.
	protocols do: 
			[:each | 
			stream nextPutAll: each;
				nextPut: $ ].
	^stream contents
]

{ #category : #testing }
RBProtocolEnvironment >> definesClass: aClass [
	^ aClass == class and: [super definesClass: aClass]
]

{ #category : #testing }
RBProtocolEnvironment >> includesCategory: aCategory [
	^ (super includesCategory: aCategory) and: [
		(environment classNamesFor: aCategory)
			inject: false into: [ :bool :each |
				bool or: [
					| aClass |
					aClass := self systemDictionary at: each ifAbsent: [ nil ].
					aClass == class or: [ aClass class == class ] ] ] ]
]

{ #category : #testing }
RBProtocolEnvironment >> includesClass: aClass [
	^ aClass == class and: [super includesClass: aClass]
]

{ #category : #testing }
RBProtocolEnvironment >> includesProtocol: aProtocol in: aClass [
	^aClass == class and: [(super includesProtocol: aProtocol in: aClass)
			and: [protocols includes: aProtocol]]
]

{ #category : #testing }
RBProtocolEnvironment >> includesSelector: aSelector in: aClass [ 
	^(super includesSelector: aSelector in: aClass) and: 
			[protocols 
				includes: (environment whichProtocolIncludes: aSelector in: aClass)]
]

{ #category : #testing }
RBProtocolEnvironment >> isEmpty [
	^protocols isEmpty
]

{ #category : #testing }
RBProtocolEnvironment >> packages [
	^ self methods collect: [ :m | m package ]
]

{ #category : #copying }
RBProtocolEnvironment >> postCopy [
	super postCopy.
	protocols := protocols copy
]

{ #category : #printing }
RBProtocolEnvironment >> storeOn: aStream [ 
	aStream nextPut: $(.
	super storeOn: aStream.
	aStream
		nextPutAll: ' class: ';
		nextPutAll: class name;
		nextPutAll: ' protocols: '.
	protocols asArray storeOn: aStream.
	aStream nextPut: $)
]
