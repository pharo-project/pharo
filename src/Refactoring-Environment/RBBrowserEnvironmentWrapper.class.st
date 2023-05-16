"
I am a wrapper around special browser environment subclasses and
the base RBBrowserEnvironment class. I define common methods
for my subclasses to act as a full environment.
no public use.
"
Class {
	#name : #RBBrowserEnvironmentWrapper,
	#superclass : #RBBrowserEnvironment,
	#instVars : [
		#environment
	],
	#category : #'Refactoring-Environment'
}

{ #category : #'instance creation' }
RBBrowserEnvironmentWrapper class >> new [
	^ self onEnvironment: RBBrowserEnvironment new
]

{ #category : #'instance creation' }
RBBrowserEnvironmentWrapper class >> onEnvironment: anEnvironment [
	^(self basicNew)
		initialize;
		onEnvironment: anEnvironment;
		yourself
]

{ #category : #visiting }
RBBrowserEnvironmentWrapper >> acceptVisitor: aProgramNodeVisitor [

	self subclassResponsibility
]

{ #category : #accessing }
RBBrowserEnvironmentWrapper >> basisObjects [

	^ #()
]

{ #category : #testing }
RBBrowserEnvironmentWrapper >> definesClass: aClass [
	^ environment definesClass: aClass
]

{ #category : #description }
RBBrowserEnvironmentWrapper >> description [

	^ label ifNil: [super description, self descriptionBasis]
]

{ #category : #description }
RBBrowserEnvironmentWrapper >> descriptionBasis [

	| basis |
 	basis := self basisObjects.
 	basis ifEmpty: [ ^ '' ].
 	^ String streamContents: [ :s |
 		  s nextPutAll: ': '.
 		  basis do: [ :each |
 			  self printBasisObject: each on: s.
 			  s nextPutAll: ', ' ].
 		  s skip: -2 ]
]

{ #category : #private }
RBBrowserEnvironmentWrapper >> environment [
	^ environment
]

{ #category : #testing }
RBBrowserEnvironmentWrapper >> includesCategory: aCategory [
	^environment includesCategory: aCategory
]

{ #category : #testing }
RBBrowserEnvironmentWrapper >> includesClass: aClass [
	^environment includesClass: aClass
]

{ #category : #testing }
RBBrowserEnvironmentWrapper >> includesPackage: aRPackage [
	^ self packages includes: aRPackage
]

{ #category : #testing }
RBBrowserEnvironmentWrapper >> includesProtocol: aProtocol in: aClass [
	^(self includesClass: aClass)
		and: [environment includesProtocol: aProtocol in: aClass]
]

{ #category : #testing }
RBBrowserEnvironmentWrapper >> includesSelector: aSelector in: aClass [
	^(self includesClass: aClass)
		and: [environment includesSelector: aSelector in: aClass]
]

{ #category : #testing }
RBBrowserEnvironmentWrapper >> isBoundToEnvironment [

	^ environment notNil
]

{ #category : #testing }
RBBrowserEnvironmentWrapper >> isEmpty [
	self classesDo: [:each | ^false].
	^true
]

{ #category : #testing }
RBBrowserEnvironmentWrapper >> isSystem [
	^false
]

{ #category : #initialization }
RBBrowserEnvironmentWrapper >> onEnvironment: anEnvironment [
	environment := anEnvironment
]

{ #category : #private }
RBBrowserEnvironmentWrapper >> packageNames [
	^ self packages collect: [ :each | each packageName ]
]

{ #category : #accessing }
RBBrowserEnvironmentWrapper >> packages [

	^ self subclassResponsibility
]

{ #category : #printing }
RBBrowserEnvironmentWrapper >> printBasisObject: anObject on: aStream [

	^ anObject printOn: aStream
]

{ #category : #private }
RBBrowserEnvironmentWrapper >> rootEnvironment [
	^ environment rootEnvironment
]

{ #category : #accessing }
RBBrowserEnvironmentWrapper >> selectionIntervalFor: aString [
	| interval |
	interval := super selectionIntervalFor: aString.
	^interval ifNotNil: [interval]
		ifNil: [environment selectionIntervalFor: aString]
]

{ #category : #printing }
RBBrowserEnvironmentWrapper >> storeOn: aStream [
	aStream
		nextPutAll: '(';
		nextPutAll: self class name;
		nextPutAll: ' onEnvironment: '.
	environment storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #private }
RBBrowserEnvironmentWrapper >> systemDictionary [
	^ environment systemDictionary
]
