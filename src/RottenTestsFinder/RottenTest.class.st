"
I model a rotten test. I hold the CompiledMethod concerned.
"
Class {
	#name : 'RottenTest',
	#superclass : 'Object',
	#instVars : [
		'compiledMethod',
		'callSites'
	],
	#category : 'RottenTestsFinder-Results',
	#package : 'RottenTestsFinder',
	#tag : 'Results'
}

{ #category : 'instance creation' }
RottenTest class >> compiledMethod: aCompiledMethod [
	^ self new
		compiledMethod: aCompiledMethod;
		yourself
]

{ #category : 'comparing' }
RottenTest >> = anObject [
	^ self class = anObject class
		and: [ self compiledMethod = anObject compiledMethod ]
]

{ #category : 'visiting' }
RottenTest >> acceptVisitor: aRTFResultsVisitor [
	^ aRTFResultsVisitor visitRottenTest: self
]

{ #category : 'rotten tests set' }
RottenTest >> addYourselfIn: aCollection [
	aCollection add: self
]

{ #category : 'accessing' }
RottenTest >> allRottenCompiledMethods [
	^ { self compiledMethod }
]

{ #category : 'accessing' }
RottenTest >> callSites [
	^ callSites
]

{ #category : 'accessing' }
RottenTest >> callSites: anObject [
	callSites := anObject
]

{ #category : 'accessing' }
RottenTest >> compiledMethod [
	^ compiledMethod
]

{ #category : 'accessing' }
RottenTest >> compiledMethod: aCompiledMethod [
	compiledMethod := aCompiledMethod
]

{ #category : 'comparing' }
RottenTest >> hash [
	^ self compiledMethod hash
]

{ #category : 'testing' }
RottenTest >> isImplementedInSuperClass [
	^ false
]

{ #category : 'testing' }
RottenTest >> isImplementedInTrait [
	^ false
]

{ #category : 'testing' }
RottenTest >> isRottenTestHelper [
	^ false
]

{ #category : 'printing' }
RottenTest >> printOn: aStream [
	super printOn: aStream.
	aStream << $(.
	self compiledMethod printOn: aStream.
	aStream << $)
]
