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

	aWorkingCopy packageSet systemCategories do: [ :packageName |
		(Smalltalk organization classesInPackageNamed: packageName) isEmpty ifTrue: [ Smalltalk organization removeSystemCategory: packageName ] ]
]

{ #category : #cleaning }
GoferCleanup >> cleanupProtocols: aWorkingCopy [

	aWorkingCopy packageSet extendedClasses do: [ :class |
		(aWorkingCopy packageSet extensionCategoriesForClass: class) do: [ :category |
			(class organization methodsInProtocolNamed: category) isEmpty ifTrue: [ class organization removeProtocolNamed: category ] ] ].

	aWorkingCopy packageSet classesAndMetaClasses do: [ :class |
		(aWorkingCopy packageSet coreCategoriesForClass: class) do: [ :category |
			(class organization methodsInProtocolNamed: category) isEmpty ifTrue: [ class organization removeProtocolNamed: category ] ] ]
]

{ #category : #running }
GoferCleanup >> execute [

	self workingCopies
		do: [ :each | self cleanup: each ]
]
