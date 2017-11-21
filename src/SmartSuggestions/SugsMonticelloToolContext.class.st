"
I am a general suggestion context for a monticello tools. I give the Suggestion framework information about the selection so it can propose the correct suggestions.
"
Class {
	#name : #SugsMonticelloToolContext,
	#superclass : #SugsAbstractContext,
	#category : #SmartSuggestions-Menu
}

{ #category : #selection }
SugsMonticelloToolContext >> selectedMethod [
	
	^ nil
]

{ #category : #refactoring }
SugsMonticelloToolContext >> sourceTextArea [
	^ model window submorphs detect: [:ea | ea respondsTo: #getTextSelector ].
]
