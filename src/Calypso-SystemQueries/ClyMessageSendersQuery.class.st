"
I return all senders of any of given selectors
"
Class {
	#name : #ClyMessageSendersQuery,
	#superclass : #ClyMessageBasedQuery,
	#instVars : [
		'specialSelectorIndexes'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyMessageSendersQuery >> description [

	^'senders of ', self printSelectors
]

{ #category : #execution }
ClyMessageSendersQuery >> prepareStateBeforeExecution [
	super prepareStateBeforeExecution.
	
	"Special selector indexes are required for lookup in method.
	Here it is collected in advance to be used during execution
	because after execution query become readonly object"
	self specialSelectorIndexes
]

{ #category : #testing }
ClyMessageSendersQuery >> selectsMethod: aMethod [
	
	selectors with: self specialSelectorIndexes do: [ :selector :specialIndex |
		(aMethod hasSelector: selector specialSelectorIndex: specialIndex) 
			ifTrue: [	^true] ].
			
	^false
]

{ #category : #accessing }
ClyMessageSendersQuery >> specialSelectorIndexes [
	^specialSelectorIndexes ifNil: [ 
		specialSelectorIndexes := selectors collect: [:each | Smalltalk specialSelectorIndexOrNil: each]]
]
