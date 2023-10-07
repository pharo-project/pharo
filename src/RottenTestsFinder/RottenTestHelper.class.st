"
I model a rotten test helper. That is to say, a method that is not a test, doing assertion and called during the execution of a test.
"
Class {
	#name : 'RottenTestHelper',
	#superclass : 'RottenTest',
	#instVars : [
		'testHelperCompiledMethods'
	],
	#category : 'RottenTestsFinder-Results',
	#package : 'RottenTestsFinder',
	#tag : 'Results'
}

{ #category : 'instance creation' }
RottenTestHelper class >> compiledMethod: compiledMethod testHelperCompiledMethods: testHelperCompiledMethodsRun [
	^ (self compiledMethod: compiledMethod)
			testHelperCompiledMethods: testHelperCompiledMethodsRun;
			yourself
]

{ #category : 'visiting' }
RottenTestHelper >> acceptVisitor: aRTFResultsVisitor [
	^ aRTFResultsVisitor visitRottenTestHelper: self
]

{ #category : 'testing' }
RottenTestHelper >> isRottenTestHelper [
	^ true
]

{ #category : 'accessing' }
RottenTestHelper >> testHelperCompiledMethods [
	^ testHelperCompiledMethods
]

{ #category : 'accessing' }
RottenTestHelper >> testHelperCompiledMethods: anObject [
	testHelperCompiledMethods := anObject
]
