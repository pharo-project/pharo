"
Cleanup working copies
"
Class {
	#name : 'GoferCleanup',
	#superclass : 'GoferWorking',
	#category : 'Gofer-Deprecated-Operations',
	#package : 'Gofer-Deprecated',
	#tag : 'Operations'
}

{ #category : 'cleaning' }
GoferCleanup >> cleanup: aWorkingCopy [

	aWorkingCopy systemPackage ifNotNil: [ :package |
		package removeEmptyTags.
		package isEmpty ifTrue: [ package removeFromSystem ] ]
]

{ #category : 'running' }
GoferCleanup >> execute [

	self workingCopies do: [ :workingCopy | self cleanup: workingCopy ]
]
