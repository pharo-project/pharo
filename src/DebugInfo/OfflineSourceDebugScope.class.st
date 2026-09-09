"
I represent a lexical scope in a method.

I provide information about variables available in the scope and how to read them.
I'm implemented as a simple array of mappings.
"
Class {
	#name : 'OfflineSourceDebugScope',
	#superclass : 'Object',
	#instVars : [
		'variables',
		'children',
		'rangeStart',
		'rangeStop'
	],
	#category : 'DebugInfo',
	#package : 'DebugInfo'
}

{ #category : 'instance creation' }
OfflineSourceDebugScope class >> on: anOCMethodScope [

	^ self new
		  astScope: anOCMethodScope;
		  yourself
]

{ #category : 'accessing' }
OfflineSourceDebugScope >> addChild: aCachedDebugScope [ 
	
	children ifNil: [ children := #() ].
	children := children copyWith: aCachedDebugScope
]

{ #category : 'accessing' }
OfflineSourceDebugScope >> bytecodeRange [

	^ rangeStart to: rangeStop
]

{ #category : 'accessing' }
OfflineSourceDebugScope >> bytecodeRange: aRange [

	rangeStart := aRange first.
	rangeStop := aRange last
]

{ #category : 'accessing' }
OfflineSourceDebugScope >> children [

	^ children ifNil: [ #() ]
]

{ #category : 'api' }
OfflineSourceDebugScope >> debugScopeAt: aBytecodePC [

	"Precondition: this method is only valid if I match the argument PC"

	self children do: [ :child |
		(child matchesBytecodePC: aBytecodePC)
			ifTrue: [ ^ child debugScopeAt: aBytecodePC ] ].
	
	"If none of my children match, then it's me that matches."
	^ self
]

{ #category : 'api' }
OfflineSourceDebugScope >> isArgumentNamed: aName [

	| variableDescriptor |
	variableDescriptor := self variables
		detect: [ :e | e first = aName ]
		ifNone: [ NotFound signal: 'Variable not found: ', aName ].
	^ variableDescriptor second
]

{ #category : 'api' }
OfflineSourceDebugScope >> matchesBytecodePC: aPC [

	^ self bytecodeRange includes: aPC
]

{ #category : 'printing' }
OfflineSourceDebugScope >> printOn: aStream [
	"Generate a string representation of the receiver based on its instance variables."

	super printOn: aStream.
	aStream
		nextPutAll: '(';
		print: rangeStart;
		nextPutAll: ' - ';
		print: rangeStop;
		nextPutAll: ')'
]

{ #category : 'api' }
OfflineSourceDebugScope >> readVariableNamed: aName fromContext: aContext [

	| variableDescriptor definingContext tempValue |
	variableDescriptor := self variables
		detect: [ :e | e first = aName ]
		ifNone: [ self error: 'Variable not found: ', aName  ].
	definingContext := aContext.
	variableDescriptor third timesRepeat: [
		definingContext := aContext outerContext ].

	tempValue := definingContext tempAt: variableDescriptor fourth.
	"If it is a temp vector, lookup inside"
	variableDescriptor size > 4 ifTrue: [
		tempValue := tempValue at: variableDescriptor fifth ].
	^ tempValue
]

{ #category : 'accessing' }
OfflineSourceDebugScope >> variableNames [
	
	^ self variables collect: [ :each | each first ]
]

{ #category : 'accessing' }
OfflineSourceDebugScope >> variables [
	
	^ variables ifNil: [ #() ]
]

{ #category : 'accessing' }
OfflineSourceDebugScope >> variables: aCollection [ 
	variables := aCollection
]
