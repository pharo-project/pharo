"
I return all senders of any of given selectors
"
Class {
	#name : #ClyMessageSenders,
	#superclass : #ClyMessageBasedQuery,
	#instVars : [
		'specialSelectorIndexes'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyMessageSenders >> description [

	^'senders of ', self printSelectors
]

{ #category : #execution }
ClyMessageSenders >> prepareStateBeforeExecution [
	super prepareStateBeforeExecution.
	
	"Special selector indexes are required for lookup in method.
	Here it is collected in advance to be used during execution
	because after execution query become readonly object"
	self specialSelectorIndexes
]

{ #category : #testing }
ClyMessageSenders >> selectsMethod: aMethod [
	
	selectors with: self specialSelectorIndexes do: [ :selector :specialIndex |
		(aMethod hasSelector: selector specialSelectorIndex: specialIndex) 
			ifTrue: [	^true] ].
			
	^false
]

{ #category : #accessing }
ClyMessageSenders >> specialSelectorIndexes [
	^specialSelectorIndexes ifNil: [ 
		specialSelectorIndexes := selectors collect: [:each | Smalltalk specialSelectorIndexOrNil: each]]
]
