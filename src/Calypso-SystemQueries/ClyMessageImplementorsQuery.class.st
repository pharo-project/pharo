"
I return all implementors of given selectors
"
Class {
	#name : #ClyMessageImplementorsQuery,
	#superclass : #ClyMessageBasedQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyMessageImplementorsQuery >> description [

	^'implementors of ', self printSelectors 
]

{ #category : #testing }
ClyMessageImplementorsQuery >> selectsMethod: aMethod [

	"is implementor if it is implemented in the same class 
	 that is defined (to exclude trait users)"
	^ (selectors includes: aMethod selector)
		and: [  aMethod origin = aMethod methodClass ]
]
