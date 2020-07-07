Class {
	#name : #MicAnchorBlock,
	#superclass : #MicSingleLineBlock,
	#instVars : [
		'label'
	],
	#category : #'Microdown-Model'
}

{ #category : #public }
MicAnchorBlock >> addLineAndReturnNextNode: line [
	"Line is on the form '#### my heading at forth level'"
	label := (line copyFrom: parser anchorMarkup size + 1 to: line size) trim.
	^ self
]

{ #category : #accessing }
MicAnchorBlock >> label [
	^ label
]

{ #category : #accessing }
MicAnchorBlock >> label: anObject [
	label := anObject
]

{ #category : #printing }
MicAnchorBlock >> printOn: aStream [

	aStream nextPutAll: 'Anchor: '.
	aStream nextPutAll: label
]
