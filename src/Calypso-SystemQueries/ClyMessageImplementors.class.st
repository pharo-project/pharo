"
I return all implementors of given selectors
"
Class {
	#name : #ClyMessageImplementors,
	#superclass : #ClyMessageBasedQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyMessageImplementors >> description [

	^'implementors of ', self printSelectors 
]

{ #category : #testing }
ClyMessageImplementors >> selectsMethod: aMethod [
	^selectors includes: aMethod selector
]
