Class {
	#name : 'UnknownBytecode',
	#superclass : 'Error',
	#instVars : [
		'pc',
		'bytecode'
	],
	#category : 'Debugging-Core',
	#package : 'Debugging-Core'
}

{ #category : 'accessing' }
UnknownBytecode >> bytecode [
	^ bytecode
]

{ #category : 'accessing' }
UnknownBytecode >> bytecode: aValue [

	bytecode := aValue
]

{ #category : 'accessing' }
UnknownBytecode >> pc: aValue [

	pc := aValue
]
