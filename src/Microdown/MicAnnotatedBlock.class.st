"
I represent a paragraph with the title. My title can only contain a string while my body can contain formatted text.

I'm created with `!!paragraph` (such as `@@note` `@@important` in Pillar).

!!note This is important so I used exclamatation mark.

### Known limits. 
We should revisit the implementation because there is something unclear. 
Currently we cannot add list for example inside a annotatedBlock.
See [https://github.com/pillar-markup/MicroDown/issues/54](https://github.com/pillar-markup/MicroDown/issues/54)

"
Class {
	#name : #MicAnnotatedBlock,
	#superclass : #MicAbstractAnnotatedBlock,
	#category : #'Microdown-Model'
}

{ #category : #visiting }
MicAnnotatedBlock >> accept: aVisitor [
 	^ aVisitor visitAnnotated: self
]

{ #category : #testing }
MicAnnotatedBlock >> addLineAndReturnNextNode: line [
	"line is assumed to be of the form '@@label some text
	
	'
	"

	isClosed
		ifTrue: [ ^ self ].
	label 
		ifNil: [ 
			| indexOfFirstSpace |
			indexOfFirstSpace := line indexOf: Character space.
			label := line copyFrom: self lineMarkup size + 1 to: indexOfFirstSpace - 1.
			body := (line copyFrom: indexOfFirstSpace + 1 to: line size) trim ]
		ifNotNil: [ body := body , String cr , line ]
]

{ #category : #testing }
MicAnnotatedBlock >> canConsumeLine: line [
	"return if this block can consume line"

	"it can be dangerous but let us try"
	^ line isNotEmpty 
]

{ #category : #'parse support' }
MicAnnotatedBlock >> closeMe [
	super closeMe.
	body := self inlineParse: body
]

{ #category : #accessing }
MicAnnotatedBlock >> label [
	^ label
]

{ #category : #testing }
MicAnnotatedBlock >> lineMarkup [
	"Normally !!"
	^ parser annotatedParagraphMarkup
]

{ #category : #accessing }
MicAnnotatedBlock >> text [

	^ String streamContents: [ :s | self textElements do: [ :each | s nextPutAll: each substring ] ]
]

{ #category : #accessing }
MicAnnotatedBlock >> textElements [

	^ self body
]
