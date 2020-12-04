"
I wrap a HelpTopic entry, to modify its behavior if required for the spotter
"
Class {
	#name : #SptHelpEntry,
	#superclass : #SptEntry,
	#category : #'GT-Spotter-Processors-Entries'
}

{ #category : #converting }
SptHelpEntry >> asString [ 

	^ content title
]

{ #category : #evaluating }
SptHelpEntry >> doEvaluate [ 

	content spotterActDefault 
]

{ #category : #accessing }
SptHelpEntry >> icon [
	
	^ content gtTopicIcon
]

{ #category : #'spotter-extensions' }
SptHelpEntry >> spotterPreviewIn: aComposite inContext: aSpotter [

	content spotterPreviewIn: aComposite inContext: aSpotter
]
