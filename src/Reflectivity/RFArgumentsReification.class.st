"
reify args
"
Class {
	#name : 'RFArgumentsReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFArgumentsReification class >> entities [
	^{OCMessageNode. OCMethodNode. OCSequenceNode}
]

{ #category : 'plugin interface' }
RFArgumentsReification class >> key [
	^#arguments
]

{ #category : 'generate' }
RFArgumentsReification >> genForASTMessageNode [
	^OCVariableNode named: self varName
]

{ #category : 'generate' }
RFArgumentsReification >> genForASTMethodNode [
	^OCVariableNode named: self varName
]

{ #category : 'generate' }
RFArgumentsReification >> genForASTSequenceNode [
	^OCVariableNode named: self varName
]

{ #category : 'preamble' }
RFArgumentsReification >> preambleForMessage: aNode [
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := OrderedCollection new.

	(1 to: aNode numArgs) reverseWithIndexDo: [:each :index |
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			preamble add:  (RFStorePopIntoTempNode named: name).
	].

	(1 to: aNode numArgs) withIndexDo: [:each :index |
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			arguments add:  (OCVariableNode named: name).
	].
	preamble addAll: (OCArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: self varName).
	preamble addAll: arguments.
	^ preamble
]

{ #category : 'preamble' }
RFArgumentsReification >> preambleForMethod: aNode [
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := aNode argumentNames collect: [ :name | OCVariableNode named: name].
	preamble addAll: (OCArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: self varName).
	^ preamble
]

{ #category : 'preamble' }
RFArgumentsReification >> preambleSequence: aNode [
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := aNode parent argumentNames collect: [ :name | OCVariableNode named: name].
	preamble addAll: (OCArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: self varName).
	^ preamble
]

{ #category : 'generate' }
RFArgumentsReification >> varName [
	^#RFArgumentsReificationVar
]
