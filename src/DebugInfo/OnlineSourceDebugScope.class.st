"
I represent a scope wrapping a compiler scope.

I provide information about variables available in the scope and how to read them.
"
Class {
	#name : 'OnlineSourceDebugScope',
	#superclass : 'Object',
	#instVars : [
		'astScope'
	],
	#category : 'DebugInfo',
	#package : 'DebugInfo'
}

{ #category : 'instance creation' }
OnlineSourceDebugScope class >> on: anOCMethodScope [

	^ self new
		  astScope: anOCMethodScope;
		  yourself
]

{ #category : 'accessing' }
OnlineSourceDebugScope >> astScope [
	^ astScope
]

{ #category : 'accessing' }
OnlineSourceDebugScope >> astScope: anOCMethodScope [ 
	
	astScope := anOCMethodScope 
]

{ #category : 'api' }
OnlineSourceDebugScope >> isArgumentNamed: aName [

	^ (self variableNamed: aName) isArgumentVariable
]

{ #category : 'api' }
OnlineSourceDebugScope >> readVariableNamed: aName fromContext: aContext [

	^ (self variableNamed: aName) readInContext: aContext
]

{ #category : 'api' }
OnlineSourceDebugScope >> variableNamed: aName [

	^ self variableNamed: aName ifAbsent: [ self error: 'Variable not found: ' , aName ]
]

{ #category : 'api' }
OnlineSourceDebugScope >> variableNamed: aName ifAbsent: aBlock [

	^ self variables
		detect: [ :e | e name = aName ]
		ifNone: aBlock
]

{ #category : 'accessing' }
OnlineSourceDebugScope >> variableNames [
	
	^ astScope allTempNames
]

{ #category : 'accessing' }
OnlineSourceDebugScope >> variables [
	
	"Consider copied vars first.
	They override variables from the scope"
	| vars |
	vars := astScope copiedVars copy.
	astScope allTemps do: [ :e |
		vars at: e name ifAbsentPut: e.
	].
	^ vars values
]
