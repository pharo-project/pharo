"
I am command line test runner. 
I run a TestSuite and outpout the progress in a terminal friendly way with XTerm characters.
"
Class {
	#name : #VTermTestRunner,
	#superclass : #CommandLineTestRunner,
	#category : #'SUnit-UI-Headless'
}

{ #category : #private }
VTermTestRunner >> createStderr [
	^ VTermOutputDriver stderr
]

{ #category : #private }
VTermTestRunner >> createStdout [
	^ VTermOutputDriver stdout
]

{ #category : #helper }
VTermTestRunner >> print: aLongErrorMesssage short: aShortErrorMesssage of: aTestCase [ 
	| testCaseString longMessage shortMessage offset cr |
	
	cr := OSPlatform current lineEnding.
	shortMessage := aShortErrorMesssage ifNil: [ '' ].
	longMessage := aLongErrorMesssage ifNil: [ '' ].
	
	testCaseString := aTestCase printString.
	self stdout 
		startOfLine; clearToEnd; nextPutAll: testCaseString.
	
	"Let's see if we have enough space to print the error"
	offset := 80 - testCaseString size - longMessage size.
	offset < 1
		ifTrue: [ 
			self stdout startOfLine; right: (80 - shortMessage size); 
				nextPutAll: shortMessage ]
		ifFalse: [ 
			self stdout right: offset;
				nextPutAll: longMessage ].
		
	self stdout clear.
	self stdout << cr
]

{ #category : #helper }
VTermTestRunner >> printError: anError of: aTestCase [ 
	self stderr red.
	super printError: anError of: aTestCase
]

{ #category : #helper }
VTermTestRunner >> printFailure: anError of: aTestCase [ 
	self stderr yellow.
	^ super printFailure: anError of: aTestCase 
]

{ #category : #helper }
VTermTestRunner >> printPassOf: aTestCase [
]

{ #category : #printing }
VTermTestRunner >> printProgress [
	| string |
	string := '[',currentTest asString, '/', maxTest asString,']'.
	self stderr 
		startOfLine; right: (80 - string size); nextPutAll: string.
]

{ #category : #printing }
VTermTestRunner >> printTestCase: aTestCase [ 
	self stderr 
		startOfLine; clearToEnd; green; 
		print: aTestCase; clear.
	self printProgress.
]
