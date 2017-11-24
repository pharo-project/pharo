"
reify args
"
Class {
	#name : #RFArgumentsReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Links'
}

{ #category : #'plugin interface' }
RFArgumentsReification class >> entities [
	^{RBMessageNode. RBMethodNode. RBBlockNode}
]

{ #category : #'plugin interface' }
RFArgumentsReification class >> key [
	^#arguments
]

{ #category : #generate }
RFArgumentsReification >> genForRBMessageNode [
	^RBTemporaryNode named: self varName.
]

{ #category : #generate }
RFArgumentsReification >> genForRBProgramNode [
	^RBTemporaryNode named: self varName.
]

{ #category : #generate }
RFArgumentsReification >> preamble: aNode [
	^aNode isMessage 
		ifTrue: [ self preambleForMessage: aNode ]
		ifFalse: [ self preambleForMethod: aNode ]
]

{ #category : #generate }
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
			arguments add:  (RBTemporaryNode named: name).
	].
	preamble addAll: (RBArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	preamble addAll: arguments.
	^ preamble 
]

{ #category : #generate }
RFArgumentsReification >> preambleForMethod: aNode [
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := aNode argumentNames collect: [ :name | RBTemporaryNode named: name].
	preamble addAll: (RBArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	^ preamble 
]

{ #category : #generate }
RFArgumentsReification >> varName [
	^#RFArgumentsReificationVar
]
