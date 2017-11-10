Class {
	#name : #MetacelloMethodSection,
	#superclass : #Object,
	#instVars : [
		'attributes'
	],
	#category : #Metacello-Core-Constructors
}

{ #category : #accessing }
MetacelloMethodSection >> attribute [
    self deprecated: 'Use attributes instead'.
    self attributes size > 1
        ifTrue: [ self error: 'invalid use of attribute' ].
    self attributes isEmpty
        ifTrue: [ ^ nil ].
    ^ self attributes first
]

{ #category : #accessing }
MetacelloMethodSection >> attribute: anObject [
    self deprecated: 'Use attributes: instead'.
    self attributes size > 1
        ifTrue: [ self error: 'invalid use of attribute:' ].
    attributes := OrderedCollection with: anObject
]

{ #category : #printing }
MetacelloMethodSection >> attributePrintString [
    | stream |
    stream := WriteStream on: String new.
    self attributes size = 1
        ifTrue: [ stream nextPutAll: '#' , attributes first asString printString ]
        ifFalse: [ 
            stream nextPutAll: '#('.
            self attributes
                do: [ :attribute | 
                    stream
                        nextPutAll: '#' , attribute asString printString;
                        space ].
            stream nextPut: $) ].
    ^ stream contents
]

{ #category : #accessing }
MetacelloMethodSection >> attributes [
    attributes ifNil: [ attributes := OrderedCollection new ].
    ^ attributes
]

{ #category : #accessing }
MetacelloMethodSection >> attributes: aCollectionOrSymbol [
    attributes := aCollectionOrSymbol asMetacelloAttributeList
]

{ #category : #testing }
MetacelloMethodSection >> includesAttributeFrom: attributeCollection [
    ^ (attributeCollection asSet intersection: self attributes asSet) notEmpty
]
