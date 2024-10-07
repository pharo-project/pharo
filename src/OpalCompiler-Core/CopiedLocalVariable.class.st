"
A copied variable is an arg or temp var that is copied into a block that later reads this variable
"
Class {
	#name : 'CopiedLocalVariable',
	#superclass : 'LocalVariable',
	#instVars : [
		'originalVar'
	],
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'visiting' }
CopiedLocalVariable >> acceptVisitor: arg1 node: arg2 [

	^ originalVar acceptVisitor: arg1 node: arg2
]

{ #category : 'queries' }
CopiedLocalVariable >> definingNode [

	^ scope node temporaries
		  detect: [ :arg1 | arg1 variable == self ]
		  ifNone: [  ]
]

{ #category : 'accessing' }
CopiedLocalVariable >> index: arg1 [

	self scope == originalVar scope ifTrue: [ originalVar index: arg1 ].
	super index: arg1
]

{ #category : 'testing' }
CopiedLocalVariable >> isArgumentVariable [

	^ originalVar isArgumentVariable
]

{ #category : 'testing' }
CopiedLocalVariable >> isCopying [

	^ true
]

{ #category : 'testing' }
CopiedLocalVariable >> isTempVariable [

	^ originalVar isTempVariable
]

{ #category : 'read/write usage' }
CopiedLocalVariable >> isUninitialized [

	^ originalVar isUninitialized
]

{ #category : 'testing' }
CopiedLocalVariable >> isWritable [

	^ originalVar isWritable
]

{ #category : 'accessing' }
CopiedLocalVariable >> originalVar [

	^ originalVar
]

{ #category : 'accessing' }
CopiedLocalVariable >> originalVar: arg1 [

	originalVar := arg1
]

{ #category : 'debugging' }
CopiedLocalVariable >> writeFromContext: arg1 scope: arg2 value: arg3 [

	self isWritable ifFalse: [ ^ self error: 'Arguments are read only' ].
	^ arg2
		  setCopyingTempToAllScopesUpToDefTemp: originalVar
		  to: arg3
		  from: arg1
]
