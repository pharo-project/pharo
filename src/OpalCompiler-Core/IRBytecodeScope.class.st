"
I am an internal class used by the  decompiler to recreat variable information
"
Class {
	#name : #IRBytecodeScope,
	#superclass : #Object,
	#instVars : [
		'temps',
		'numArgs',
		'ownTempVectors',
		'copiedValues'
	],
	#category : 'OpalCompiler-Core-Bytecode'
}

{ #category : #accessing }
IRBytecodeScope >> args [
	| args |
	args := Array new: numArgs.
	1 to: numArgs do: [ :idx | args at: idx put: self -> (idx - 1)].
	^ args
]

{ #category : #accessing }
IRBytecodeScope >> copiedValues [
	^ copiedValues
]

{ #category : #accessing }
IRBytecodeScope >> copiedValues: someCopiedValues [
	copiedValues := someCopiedValues
]

{ #category : #initialization }
IRBytecodeScope >> initialize [
	temps := 0.
	ownTempVectors := OrderedCollection new
]

{ #category : #accessing }
IRBytecodeScope >> newTempVector: aTempVector at: offset [
	ownTempVectors add: aTempVector.
	self tempAt: offset
]

{ #category : #accessing }
IRBytecodeScope >> numArgs [
	^ numArgs
]

{ #category : #accessing }
IRBytecodeScope >> numArgs: anInteger [
	numArgs := anInteger.
	temps := temps max: numArgs
]

{ #category : #accessing }
IRBytecodeScope >> ownTempVectors [
	^ ownTempVectors
]

{ #category : #accessing }
IRBytecodeScope >> tempAt: zeroBasedIndex [
	temps := temps max: zeroBasedIndex + 1.
	^ self -> zeroBasedIndex
]

{ #category : #accessing }
IRBytecodeScope >> tempAt: remoteIndex inRemote: remoteArray [
	^ remoteIndex
]

{ #category : #accessing }
IRBytecodeScope >> temps [
	| result tempOffset numTemps |
	tempOffset := copiedValues size + numArgs.
	numTemps := temps - tempOffset.
	result := Array new: numTemps.
	tempOffset := tempOffset - 1.
	1 to: numTemps do: [ :idx | result at: idx put: self -> (tempOffset + idx)  ].
	ownTempVectors do: [ :tempVector | result at: tempVector index - tempOffset put: tempVector ].
	^ result
]
