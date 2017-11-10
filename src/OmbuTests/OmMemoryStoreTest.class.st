Class {
	#name : #OmMemoryStoreTest,
	#superclass : #OmStoreTest,
	#category : #OmbuTests
}

{ #category : #resources }
OmMemoryStoreTest >> setUpStore [	
	^ OmMemoryStore new
]

{ #category : #tests }
OmMemoryStoreTest >> testWithAll [

	| entries |
	entries := (OmMemoryStore withAll: #(a b c)) entries.
	self assertCollection: (entries collect: #content) hasSameElements: #(a b c).
]
