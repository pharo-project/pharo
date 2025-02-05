"
I am an internal class used by the  decompiler to recreat variable information
"
Class {
	#name : 'OCIRBytecodeScope',
	#superclass : 'Object',
	#instVars : [
		'temps',
		'numArgs',
		'ownTempVectors',
		'copiedValues'
	],
	#category : 'OpalCompiler-Core-Bytecode',
	#package : 'OpalCompiler-Core',
	#tag : 'Bytecode'
}

{ #category : 'accessing' }
OCIRBytecodeScope >> args [
	| args |
	args := Array new: numArgs.
	1 to: numArgs do: [ :idx | args at: idx put: self -> (idx - 1)].
	^ args
]

{ #category : 'accessing' }
OCIRBytecodeScope >> copiedValues [
	^ copiedValues
]

{ #category : 'accessing' }
OCIRBytecodeScope >> copiedValues: someCopiedValues [
	copiedValues := someCopiedValues
]

{ #category : 'initialization' }
OCIRBytecodeScope >> initialize [
	temps := 0.
	ownTempVectors := OrderedCollection new
]

{ #category : 'accessing' }
OCIRBytecodeScope >> newTempVector: aTempVector at: offset [
	ownTempVectors add: aTempVector.
	self tempAt: offset
]

{ #category : 'accessing' }
OCIRBytecodeScope >> numArgs [
	^ numArgs
]

{ #category : 'accessing' }
OCIRBytecodeScope >> numArgs: anInteger [
	numArgs := anInteger.
	temps := temps max: numArgs
]

{ #category : 'accessing' }
OCIRBytecodeScope >> ownTempVectors [
	^ ownTempVectors
]

{ #category : 'accessing' }
OCIRBytecodeScope >> tempAt: zeroBasedIndex [
	temps := temps max: zeroBasedIndex + 1.
	^ self -> zeroBasedIndex
]

{ #category : 'accessing' }
OCIRBytecodeScope >> tempAt: remoteIndex inRemote: remoteArray [
	^ remoteIndex
]

{ #category : 'accessing' }
OCIRBytecodeScope >> temps [
	| result tempOffset numTemps |
	tempOffset := copiedValues size + numArgs.
	numTemps := temps - tempOffset.
	result := Array new: numTemps.
	tempOffset := tempOffset - 1.
	1 to: numTemps do: [ :idx | result at: idx put: self -> (tempOffset + idx)  ].
	ownTempVectors do: [ :tempVector | result at: tempVector index - tempOffset put: tempVector ].
	^ result
]
