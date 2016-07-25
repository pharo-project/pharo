dispatching
dispatchMouseDown: anEvent with: aMorph
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