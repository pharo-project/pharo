"
I'm the background style of a VTermOutputDriver
I'm characterized by the color of the background that will be printed.
"
Class {
	#name : 'VTermOutputBackground',
	#superclass : 'VTermOutputStyleElement',
	#pools : [
		'VTermOutputStyles'
	],
	#category : 'System-CommandLine-VTerm-Styling',
	#package : 'System-CommandLine',
	#tag : 'VTerm-Styling'
}

{ #category : 'comparing' }
VTermOutputBackground >> = aBackground [
	^ self color isVTermDefaultColor and: [ aBackground color isVTermDefaultColor ]
]

{ #category : 'comparing' }
VTermOutputBackground >> hash [
	^ self color hash
]
