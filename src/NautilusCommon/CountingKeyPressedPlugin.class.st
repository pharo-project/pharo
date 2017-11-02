"
I'm a simple plugin which displays the number of keystrokes at the bottom of Nautilus. I'm mainly an example.
"
Class {
	#name : #CountingKeyPressedPlugin,
	#superclass : #AbstractKeyPressedPlugin,
	#instVars : [
		'stringMorph',
		'counter'
	],
	#category : 'NautilusCommon-Plugin-Examples'
}

{ #category : #position }
CountingKeyPressedPlugin class >> defaultPosition [

	^ #bottom
]

{ #category : #information }
CountingKeyPressedPlugin class >> description [

	^ 'Display the number of key pressed'
]

{ #category : #display }
CountingKeyPressedPlugin >> display [

	^ stringMorph
]

{ #category : #initialization }
CountingKeyPressedPlugin >> initialize [
	super initialize.
	counter := 0.
	self stringMorph
		contents: 'Keys pressed: ' , counter printString;
		openInWorld
]

{ #category : #announcement }
CountingKeyPressedPlugin >> keyPressed: anAnnouncement [
	counter := counter + 1.
	self stringMorph contents: 'Keys pressed: ' , counter printString
]

{ #category : #accessing }
CountingKeyPressedPlugin >> stringMorph [
	^ stringMorph
		ifNil: [ 
			stringMorph := LabelMorph new
				enabled: false;
				vResizing: #shrinkWrap;
				hResizing: #spaceFill;
				yourself ]
]

{ #category : #accessing }
CountingKeyPressedPlugin >> stringMorph: anObject [
	stringMorph := anObject
]
