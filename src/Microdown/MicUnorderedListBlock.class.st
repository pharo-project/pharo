"
I represent an unordered list of list items.

#### Example

Typing
```
- first
- second
  on multiple line 
- third
Not in third
- first item of a new list
```

produces
- first
- second
  on multiple line 
- third
Not in third
- first item of a new list

Notice how the following line of the second item is indented with the previous one.
"
Class {
	#name : #MicUnorderedListBlock,
	#superclass : #MicListBlock,
	#category : #'Microdown-Model'
}

{ #category : #testing }
MicUnorderedListBlock >> canConsumeLine: line [
	"to consume this line there must be a UnorderdListItem start at the right indentation"

	line ifEmpty: [ ^ true ].
	^ (super canConsumeLine: line) and: [line trim matchesRegex: '(^\* |^- | ^\+).*']
]

{ #category : #printing }
MicUnorderedListBlock >> printOn: aStream [

	aStream nextPutAll: 'UnorderedList'.
	
]
