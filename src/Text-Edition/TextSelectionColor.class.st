"
I represent a text selection attribute that can be added to a text for a text emphazing (text color). I have a color block (colorBlock) which is evaluated in order to get the text color. 
I have three instances, one for the primary selection and the two others for the secondary selection and the find replace selection. 
PrimarySelection is used in TextEditor>>#storeSelectionInParagraph in order to emphasize the selection text if the selection text color setting is not nil.
SecondarySelection and FindReplaceSelection are manage by MultiNewParagraph if secondary selection text and find replace selection text colors are not nil.

Instance Variables
	colorBlock:		<Block>

colorBlock
	- The block which is evaluated in order to get the text color

"
Class {
	#name : #TextSelectionColor,
	#superclass : #TextAttribute,
	#instVars : [
		'colorBlock'
	],
	#classVars : [
		'FindReplaceSelection',
		'PrimarySelection',
		'SecondarySelection'
	],
	#category : #Text-Edition
}

{ #category : #cleaning }
TextSelectionColor class >> cleanUp [
	PrimarySelection := nil.
]

{ #category : #'default accessing' }
TextSelectionColor class >> findReplaceSelection [
	^ FindReplaceSelection
		ifNil: [FindReplaceSelection := self new
						colorBlock: [self theme currentSettings findReplaceSelectionTextColor]]
]

{ #category : #'default accessing' }
TextSelectionColor class >> primarySelection [
	^ PrimarySelection
		ifNil: [PrimarySelection := self new colorBlock: [ self theme currentSettings selectionTextColor]]
]

{ #category : #'default accessing' }
TextSelectionColor class >> secondarySelection [
	^ SecondarySelection
		ifNil: [SecondarySelection := self new
						colorBlock: [self theme currentSettings secondarySelectionTextColor]] 
]

{ #category : #'default accessing' }
TextSelectionColor class >> theme [
	^ Smalltalk ui theme
]

{ #category : #accessing }
TextSelectionColor >> colorBlock [

	^ colorBlock ifNil: [colorBlock := [self theme currentSettings selectionTextColor]]
]

{ #category : #accessing }
TextSelectionColor >> colorBlock: aBlock [
	"aBlock value gives the text color"
	colorBlock := aBlock
]

{ #category : #accessing }
TextSelectionColor >> dominates: other [
	"There can be only one..."
	^self class == other class
]

{ #category : #accessing }
TextSelectionColor >> emphasizeScanner: scanner [ 
	"Set the emphasis for text display"
	scanner textColor: self textColor
]

{ #category : #accessing }
TextSelectionColor >> textColor [
	^ self colorBlock value
]

{ #category : #accessing }
TextSelectionColor >> theme [
	^ Smalltalk ui theme
]
