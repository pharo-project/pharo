"
I'm a style to control the font of a widget. 
I keep properties like 

- name -> The font name 
- size -> The font size
"
Class {
	#name : #SpStyleFont,
	#superclass : #SpStyleProperty,
	#instVars : [
		'name',
		'size'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpStyleFont class >> stonName [
	
	^ 'Font'
]

{ #category : #operations }
SpStyleFont >> applyTo: aMorph [

	"not all morphs understand font, I neet to verify it (which is not cool verifying a 
	 selector but other way would be to define #font: in Morph and I do not want that)"
	(aMorph respondsTo: #font:) ifFalse: [ ^ self ].
	
	aMorph font: self definedFont
]

{ #category : #operations }
SpStyleFont >> definedFont [

	^ LogicalFont
		familyName: self name 
		pointSize: self size
]

{ #category : #accessing }
SpStyleFont >> name [

	^ name
]

{ #category : #accessing }
SpStyleFont >> name: aString [

	name := aString
]

{ #category : #accessing }
SpStyleFont >> size [

	^ size
]

{ #category : #accessing }
SpStyleFont >> size: aNumber [

	size := aNumber
]
