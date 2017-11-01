"
The class represents a strategy for dispatching events to some immediate child of a morph. It is used by morphs to delegate the somewhat complex action of dispatching events accurately. 
"
Class {
	#name : #MorphicEventDispatcher,
	#superclass : #Object,
	#instVars : [
		'morph'
	],
	#category : 'Morphic-Core-Events'
}

{ #category : #dispatching }
MorphicEventDispatcher >> dispatchDefault: anEvent with: aMorph [
	"Dispatch the given event. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| localEvt index child morphs inside |
	"See if we're fully outside aMorphs bounds"
	(aMorph fullBounds containsPoint: anEvent position) ifFalse:[^#rejected]. "outside"
	"Traverse children"
	index := 1.
	morphs := aMorph submorphs.
	inside := false.
	[index <= morphs size] whileTrue:[
		child := morphs at: index.
		localEvt := anEvent transformedBy: (child transformedFrom: aMorph).
		(child processEvent: localEvt using: self) == #rejected ifFalse:[
			"Not rejected. The event was in some submorph of the receiver"
			inside := true.
			localEvt wasHandled ifTrue:[anEvent copyHandlerState: localEvt].
			index := morphs size. "break"
		].
		index := index + 1.
	].

	"Check for being inside the receiver"
	inside ifFalse:[inside := aMorph containsPoint: anEvent position event: anEvent].
	inside ifTrue:[^aMorph handleEvent: anEvent].
	^#rejected

]

{ #category : #dispatching }
MorphicEventDispatcher >> dispatchDropEvent: anEvent with: aMorph [
	"Find the appropriate receiver for the event and let it handle it. The dispatch is similar to the default dispatch with one difference: Morphs are given the chance to reject an entire drop operation. If the operation is rejected, no drop will be executed."
	| inside index morphs child localEvt |
	"Try to get out quickly"
	(aMorph fullBounds containsPoint: anEvent cursorPoint)
		ifFalse:[^#rejected].
	"Give aMorph a chance to repel the dropping morph"
	aMorph rejectDropEvent: anEvent.
	anEvent wasHandled ifTrue:[^self].

	"Go looking if any of our submorphs wants it"
	index := 1.
	inside := false.
	morphs := aMorph submorphs.
	[index <= morphs size] whileTrue:[
		child := morphs at: index.
		localEvt := anEvent transformedBy: (child transformedFrom: aMorph).
		(child processEvent: localEvt using: self) == #rejected ifFalse:[
			localEvt wasHandled ifTrue:[^anEvent wasHandled: true]. "done"
			inside := true.
			index := morphs size]. "break"
		index := index + 1.
	].

	inside ifFalse:[inside := aMorph containsPoint: anEvent cursorPoint event: anEvent].
	inside ifTrue:[^aMorph handleEvent: anEvent].
	^#rejected
]

{ #category : #dispatching }
MorphicEventDispatcher >> dispatchEvent: anEvent with: aMorph [
	"Dispatch the given event for a morph that has chosen the receiver to dispatch its events. "
	morph := aMorph.
	[ ^ anEvent sentTo: self ]
	ensure: [ morph := nil ]
]

{ #category : #dispatching }
MorphicEventDispatcher >> dispatchMouseDown: anEvent with: aMorph [
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
"
	| globalPt localEvt index child morphs handler inside lastHandler |
	"Try to get out quickly"
	globalPt := anEvent cursorPoint.
	(aMorph fullBounds containsPoint: globalPt) ifFalse:[^#rejected].

	"Install the prospective handler for the receiver"
	lastHandler := anEvent handler. "in case the mouse wasn't even in the receiver"
	handler := aMorph handlerForMouseDown: anEvent.
	handler ifNotNil:[anEvent handler: handler].

	"Now give our submorphs a chance to handle the event"
	index := 1.
	morphs := aMorph submorphs.
	[index <= morphs size] whileTrue:[
		child := morphs at: index.
		localEvt := anEvent transformedBy: (child transformedFrom: aMorph).
		(child processEvent: localEvt using: self) == #rejected ifFalse:[
			"Some child did contain the point so we're part of the top-most chain."
			inside := false.
			localEvt wasHandled ifTrue:[anEvent copyHandlerState: localEvt].
			index := morphs size].
		index := index + 1.
	].

	(inside == false or:[aMorph containsPoint: anEvent cursorPoint event: anEvent]) ifTrue:[
		"Receiver is in the top-most unlocked, visible chain."
		handler ifNotNil:[handler handleEvent: anEvent].
		"Note: Re-installing the handler is not really necessary but good style."
		anEvent handler: lastHandler.
		^self
	].
	"Mouse was not on receiver nor any of its children"
	anEvent handler: lastHandler.
	^#rejected
]

{ #category : #dispatching }
MorphicEventDispatcher >> dispatchWindowEvent: anEvent with: aMorph [
	"Host window events do not have a position and are only dispatched to the World"
	aMorph isWorldMorph ifFalse: [^#rejected].
	anEvent wasHandled ifTrue:[^self].
	^aMorph handleEvent: anEvent

]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleDropFiles: anEvent [ 
	^ self dispatchDefault: anEvent with: morph
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleDropMorph: anEvent [ 

	^ self dispatchDropEvent: anEvent with: morph
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleKeyDown: anEvent [ 

		^ self dispatchDefault: anEvent with: morph
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleKeyUp: anEvent [ 

	^ self dispatchDefault: anEvent with: morph	
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleKeystroke: anEvent [ 

	^ self dispatchDefault: anEvent with: morph
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleMouseDown: anEvent [ 

	^ self dispatchMouseDown: anEvent with: morph
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleMouseEnter: anEvent [ 

	^ self dispatchDefault: anEvent with: morph
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleMouseLeave: anEvent [ 
	^ self dispatchDefault: anEvent with: morph
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleMouseMove: anEvent [ 
	^ self dispatchDefault: anEvent with: morph
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleMouseOver: anEvent [ 	
	^ self dispatchDefault: anEvent with: morph
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleMouseUp: anEvent [ 
	^ self dispatchDefault: anEvent with: morph	
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleMouseWheel: anEvent [ 
	^ self dispatchDefault: anEvent with: morph
	
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleStep: anEvent [ 
	^ self dispatchDefault: anEvent with: morph
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleUnknownEvent: anEvent [ 
	^ self dispatchDefault: anEvent with: morph
]

{ #category : #dispatching-callback }
MorphicEventDispatcher >> handleWindowEvent: anEvent [ 

	^ self dispatchWindowEvent: anEvent with: morph
]
