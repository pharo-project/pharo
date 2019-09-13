"
Tests for FFITypeArray
"
Class {
	#name : #FFITypeArrayTest,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #tests }
FFITypeArrayTest >> testAt [
	| externalArray |
	
	externalArray := (FFITypeArray ofType: 'int' size: 10) new.
	self assert: externalArray size equals: 10.
	1 to: 10 do: [ :index | self assert: (externalArray at: index) equals: 0 ]

]

{ #category : #tests }
FFITypeArrayTest >> testAtPut [
	| externalArray |
	
	externalArray := (FFITypeArray ofType: 'int' size: 1) new.
	externalArray at: 1 put: 42.
	self assert: (externalArray at: 1) equals: 42
]

{ #category : #tests }
FFITypeArrayTest >> testExternalAt [
	| externalArray |
	
	[
	 	externalArray := (FFITypeArray ofType: 'int' size: 10) externalNew.
		self assert: externalArray size equals: 10.
		"External is not initialized, so I cannot check for contents unless I assign them"
		1 to: 10 do: [ :index | externalArray at: index put: 0 ].
		"Testing accessing"
		1 to: 10 do: [ :index | self assert: (externalArray at: index) equals: 0 ].
	] ensure: [ 
		externalArray free ] 

]

{ #category : #tests }
FFITypeArrayTest >> testExternalAtPut [
	| externalArray |
	
	[	
		externalArray := (FFITypeArray ofType: 'int' size: 1) externalNew.
		externalArray at: 1 put: 42.
		self assert: (externalArray at: 1) equals: 42 ]
	ensure: [ 
		externalArray free ]
]

{ #category : #tests }
FFITypeArrayTest >> testExternalAtPutFloat [
	| random array |
	random := Random new.
	[
		array := (FFITypeArray ofType: 'float' size: 10) externalNew.
		1 to: 10 do: [ :index | array at: index put: random next ].
		1 to: 10 do: [ :index | self assert: (array at: index) isFloat ] ]
	ensure: [ array free ]
]
