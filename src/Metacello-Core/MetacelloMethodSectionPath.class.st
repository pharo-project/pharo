Class {
	#name : #MetacelloMethodSectionPath,
	#superclass : #Array,
	#type : #variable,
	#category : #Metacello-Core-Constructors
}

{ #category : #conversion }
MetacelloMethodSectionPath >> asAttributeOrPath [
    self size = 1
        ifTrue: [ 
            | attribute |
            attribute := self at: 1.
            ^ attribute size = 1
                ifTrue: [ attribute at: 1 ]
                ifFalse: [ attribute ] ].
    ^ self
]

{ #category : #conversion }
MetacelloMethodSectionPath >> asMetacelloAttributePath [
    ^ self
]
