"
A reader for Smalltalk code
"
Class {
	#name : #MCFileTreeStReader,
	#superclass : #MCFileTreeAbstractReader,
	#category : #MonticelloFileTree-Core
}

{ #category : #accessing }
MCFileTreeStReader class >> extension [
	^ 'tree'
]

{ #category : #utilities }
MCFileTreeStReader >> addClassAndMethodDefinitionsFromDirectory: aDirectory [
    self addClassAndMethodDefinitionsFromDirectoryEntries: aDirectory entries
]
