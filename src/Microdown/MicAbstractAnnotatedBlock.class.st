Class {
	#name : #MicAbstractAnnotatedBlock,
	#superclass : #MicAbstractBlock,
	#instVars : [
		'label',
		'isClosed',
		'body'
	],
	#category : #'Microdown-Model'
}

{ #category : #testing }
MicAbstractAnnotatedBlock class >> isAbstract [

	^ self == MicAbstractAnnotatedBlock
]

{ #category : #testing }
MicAbstractAnnotatedBlock >> addLineAndReturnNextNode: line [
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

{ #category : #accessing }
MicAbstractAnnotatedBlock >> body [
	^ body
]

{ #category : #testing }
MicAbstractAnnotatedBlock >> canConsumeLine: line [
	"return if this block can consume line"

	"it can be dangerous but let us try"
	^ line isNotEmpty 
]

{ #category : #initialization }
MicAbstractAnnotatedBlock >> initialize [

	super initialize.
	isClosed := false.
]

{ #category : #accessing }
MicAbstractAnnotatedBlock >> label [
	^ label
]

{ #category : #testing }
MicAbstractAnnotatedBlock >> lineMarkup [

	^ self subclassResponsibility
]
