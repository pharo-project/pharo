"
I'm the font style of a VTermOutputDriver
I'm characterized by the color of the font that will be printed.
"
Class {
	#name : #VTermOutputFont,
	#superclass : #VTermOutputStyleElement,
	#category : 'System-CommandLine'
}

{ #category : #comparing }
VTermOutputFont >> = aFont [

	^self color isVTermDefaultColor and: [ aFont color isVTermDefaultColor ]
]

{ #category : #comparing }
VTermOutputFont >> hash [
	^ self color hash
]

{ #category : #styling }
VTermOutputFont >> setInDriver: aVTermOutputDriver [
	aVTermOutputDriver setFontColor: color.
]
