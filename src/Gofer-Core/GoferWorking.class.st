"
Common superclass for workin copies in Gofer
"
Class {
	#name : #GoferWorking,
	#superclass : #GoferOperation,
	#instVars : [
		'workingCopies'
	],
	#category : #Gofer-Core
}

{ #category : #private }
GoferWorking >> addReference: aReference [
	| workingCopy |
	workingCopy := aReference workingCopyIfAbsent: [ ^ self ].
	(self workingCopies includes: workingCopy)
		ifTrue: [ ^ self ].
	self workingCopies addLast: workingCopy.
	workingCopy requiredPackages do: [ :package |
		self addReference: (GoferPackageReference
			name: package name) ]
]

{ #category : #initialization }
GoferWorking >> initialize [
	super initialize.
	workingCopies := OrderedCollection new
]

{ #category : #initialization }
GoferWorking >> initializeOn: aGofer [
	super initializeOn: aGofer.
	aGofer references 
		do: [ :each | self addReference: each ]
]

{ #category : #accessing }
GoferWorking >> workingCopies [
	"Answer the working copies to be operated on."

	^ workingCopies
]
