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

	| packageOrganizer |
	packageOrganizer := Smalltalk organization.
	aWorkingCopy packageSet systemCategories
		select: [ :category | (packageOrganizer classesInCategory: category) isEmpty ]
		thenDo: [ :category | packageOrganizer removeSystemCategory: category ]
]

{ #category : #running }
GoferCleanup >> execute [

	self workingCopies do: [ :workingCopy | self cleanup: workingCopy ]
]
