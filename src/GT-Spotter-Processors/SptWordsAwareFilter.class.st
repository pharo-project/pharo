"
I implement a complex filter on top of my inner iterator.
I will take the filtering text and split it on the spaces, $: and camel case syntax to try to detect all the elements that have those words.

My iterator is query with the first word of the filtering text and the filtering the elements that have all the words in any part. 
For example, doing the query StoreDisk will answer also DiskStore 
"
Class {
	#name : #SptWordsAwareFilter,
	#superclass : #SptIteratorDecorator,
	#instVars : [
		'words',
		'realFilter'
	],
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #filtering }
SptWordsAwareFilter >> filteringText: aString [

	| firstWord otherWords |
	words := (aString substrings: ' :-')
		flatCollect: [ :each | each splitCamelCase  ].
	
	firstWord := words first.
	otherWords := words allButFirst collect: [:each | each asLowercase].
		
	realFilter filteringText: firstWord.
	inner := realFilter select: [ :each | 
		otherWords allSatisfy: [ :otherW | each asString asLowercase includesSubstring: otherW ] ].

]

{ #category : #accessing }
SptWordsAwareFilter >> inner: anIterator [

	inner := anIterator.
	realFilter := anIterator
]
