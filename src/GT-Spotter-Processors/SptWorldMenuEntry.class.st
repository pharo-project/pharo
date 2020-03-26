"
I wrap a World menu entry, to modify its behavior if required for the spotter
"
Class {
	#name : #SptWorldMenuEntry,
	#superclass : #SptEntry,
	#category : #'GT-Spotter-Processors-Entries'
}

{ #category : #converting }
SptWorldMenuEntry >> asString [ 

	^ content contents
]

{ #category : #evaluating }
SptWorldMenuEntry >> doEvaluate [ 

	content spotterActDefault
]

{ #category : #accessing }
SptWorldMenuEntry >> icon [

	^ content icon
]

{ #category : #'spotter-extensions' }
SptWorldMenuEntry >> spotterPreviewIn: aComposite inContext: aSpotter [

	^ GTSpotterItemBrick new
		text: content contents;
		icon: content icon;
		disable;
		yourself
]
