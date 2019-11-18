Class {
	#name : #OmMemoryStoreTest,
	#superclass : #OmStoreTest,
	#category : #'Ombu-Tests'
}

{ #category : #resources }
OmMemoryStoreTest >> setUpStore [	
	^ OmMemoryStore new
]

{ #category : #tests }
OmMemoryStoreTest >> testWithAllContents [

	| entries |
	entries := (OmMemoryStore withAllContents: #(a b c)) entries.
	self assertCollection: (entries collect: #content) hasSameElements: #(a b c).
]

{ #category : #tests }
OmMemoryStoreTest >> testWithAllEntries [

	| entries newStore |
	entries := (OmMemoryStore withAllContents: #(a b c)) entries.
	newStore := OmMemoryStore withAllEntries: entries.
	self assertCollection: newStore entries hasSameElements: entries.
]
