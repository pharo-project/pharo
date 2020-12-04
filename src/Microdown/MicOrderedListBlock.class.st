"
I represent an ordered list of list items.

#### Example

Typing 
```
1. first
2. second
   on multiple line 
1. third
Not in third
1. first item of a new list
```

produces
1. first
1. second
   on multiple line 
1. third
Not in third
1. first item of a new list

Notice how the following line of the second item is indented with the previous one.

"
Class {
	#name : #MicOrderedListBlock,
	#superclass : #MicListBlock,
	#instVars : [
		'startIndex'
	],
	#category : #'Microdown-Model'
}

{ #category : #testing }
MicOrderedListBlock >> addLineAndReturnNextNode: line [
	"Create a new item, and insert it into me."
	"line is on the format '1. item text'"

	line ifEmpty: [ ^ self ].
	self startIndexFrom: line.
	"the startindex is for the list and not for the item. 
	So it is stored once during the creation of the list and just before 
	the creation of the first item."
	^ super addLineAndReturnNextNode: line
]

{ #category : #testing }
MicOrderedListBlock >> canConsumeLine: line [
	"to consume this line there must be a UnorderdListItem start at the right indentation"
	
	line ifEmpty: [ ^ true ].
	^ (super canConsumeLine: line) and: [line trim  prefixMatchesRegex: '\d+(\.|\))']
]

{ #category : #printing }
MicOrderedListBlock >> printOn: aStream [

	aStream nextPutAll: 'OrderedList'.
	
]

{ #category : #accessing }
MicOrderedListBlock >> startIndex [
	^ startIndex
]

{ #category : #accessing }
MicOrderedListBlock >> startIndexFrom: line [
	startIndex ifNotNil: [ ^self ].
	startIndex := line asInteger
]
