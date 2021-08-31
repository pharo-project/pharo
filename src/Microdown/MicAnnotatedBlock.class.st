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

{ #category : #testing }
MicAnnotatedBlock >> lineMarkup [

	^ parser annotatedParagraphMarkup
]

{ #category : #printing }
MicAnnotatedBlock >> printOn: aStream [

	aStream nextPutAll: 'Annotated: '.
	aStream nextPutAll: (self text asString contractTo: 100)
]

{ #category : #accessing }
MicAnnotatedBlock >> text [ 

	^ self body
]
