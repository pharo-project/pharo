Class {
	#name : #MicrodownBackSlasher,
	#superclass : #Object,
	#instVars : [
		'stream',
		'allDelimiters',
		'keys'
	],
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicrodownBackSlasher >> abstractDelimiterClass [
	^ MicAbstractDelimiter
]

{ #category : #accessing }
MicrodownBackSlasher >> backSlashString: aString [

	aString doWithIndex: [ :each :i | 

		((allDelimiters includesKey: each asString) or:
			 (allDelimiters includesKey:
				  each asString , (aString at: (i + 1 min: aString size)) asString)) 
			ifTrue: [ stream nextPutAll: '\' ].
		stream nextPutAll: each asString ].
	^ stream contents
]

{ #category : #accessing }
MicrodownBackSlasher >> contents [
	^ stream contents
]

{ #category : #accessing }
MicrodownBackSlasher >> initialize [
	super initialize.
	stream := (String new: 10000) writeStream.
	allDelimiters := Dictionary new.
	self abstractDelimiterClass subclasses 
		select: [ :subclass | subclass isActive ]
		thenDo: [ :subclass | allDelimiters 
										at: subclass markup 
										put: subclass ].
	keys := allDelimiters keys.
]
