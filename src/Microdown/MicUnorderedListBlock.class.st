"
I represent an unordered list of items.

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

### Nested lists
A nested item starts aligned to its father. Below the `-` on the second level is aligned with the beginning of item1.

```
- item1
  - second level
    on two lines
- item2
```

It produces


- item1
  - second level
    on two lines
- item2


"
Class {
	#name : #MicUnorderedListBlock,
	#superclass : #MicListBlock,
	#category : #'Microdown-Model'
}

{ #category : #visiting }
MicUnorderedListBlock >> accept: aVisitor [
 	^ aVisitor visitUnorderedList: self
]

{ #category : #testing }
MicUnorderedListBlock >> canConsumeLine: line [
	"to consume this line there must be a UnorderdListItem start at the right indentation"

	line ifEmpty: [ ^ true ].
	^ (super canConsumeLine: line) and: [line trim matchesRegex: '(^\* |^- | ^\+).*']
]
