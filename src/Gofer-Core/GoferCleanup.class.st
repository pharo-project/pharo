"
Cleanup working copies
"
Class {
	#name : #GoferCleanup,
	#superclass : #GoferWorking,
	#category : #'Gofer-Core-Operations'
}

{ #category : #cleaning }
GoferCleanup >> cleanup: aWorkingCopy [

	self
		cleanupCategories: aWorkingCopy;
		cleanupProtocols: aWorkingCopy
]

{ #category : #cleaning }
GoferCleanup >> cleanupCategories: aWorkingCopy [

	aWorkingCopy packageSet systemCategories do: [ :category |
		(Smalltalk organization classesInCategory: category) isEmpty
			ifTrue: [ Smalltalk organization removeSystemCategory: category ] ]
]

{ #category : #cleaning }
GoferCleanup >> cleanupProtocols: aWorkingCopy [

	aWorkingCopy packageSet extendedClasses do: [ :class |
		(aWorkingCopy packageSet extensionProtocolsForClass: class) do: [ :protocol | class removeProtocolIfEmpty: protocol ] ].

	aWorkingCopy packageSet classesAndMetaClasses do: [ :class |
		(aWorkingCopy packageSet coreCategoriesForClass: class) do: [ :category | class removeProtocolIfEmpty: category ] ]
]

{ #category : #running }
GoferCleanup >> execute [

	self workingCopies
		do: [ :each | self cleanup: each ]
]
