"
I'm block that represents an anchor to a section. This way text can refer to the anchor to refer to the section. 

```
# A section
@mylovelySection
``` 
"
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
	"# bla
	@myanchor
	"
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
