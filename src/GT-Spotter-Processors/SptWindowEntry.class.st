"
I wrap a Window, to modify its behavior if required for the spotter
"
Class {
	#name : #SptWindowEntry,
	#superclass : #SptEntry,
	#category : #'GT-Spotter-Processors-Entries'
}

{ #category : #converting }
SptWindowEntry >> asString [ 

	^ '{1} [{2}]' format: { content label. content className }
]

{ #category : #evaluating }
SptWindowEntry >> doEvaluate [
	content spotterActDefault
]

{ #category : #accessing }
SptWindowEntry >> icon [
	
	^ content taskbarIcon
]

{ #category : #'spotter-extensions' }
SptWindowEntry >> spotterPreviewIn: aComposite inContext: aContext [

	^ content spotterPreviewIn: aComposite inContext: aContext


]
