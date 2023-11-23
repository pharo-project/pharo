"
I'm the font style of a VTermOutputDriver
I'm characterized by the color of the font that will be printed.
"
Class {
	#name : 'VTermOutputFont',
	#superclass : 'VTermOutputStyleElement',
	#category : 'System-CommandLine-VTerm-Styling',
	#package : 'System-CommandLine',
	#tag : 'VTerm-Styling'
}

{ #category : 'comparing' }
VTermOutputFont >> = aFont [

	^self color isVTermDefaultColor and: [ aFont color isVTermDefaultColor ]
]

{ #category : 'comparing' }
VTermOutputFont >> hash [
	^ self color hash
]
