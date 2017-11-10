"
MouseClickState is a simple class managing the distinction between clicks, double clicks, and drag operations. It has been factored out of HandMorph due to the many instVars.

Instance variables:
	clickClient 	<Morph>		The client wishing to receive #click:, #dblClick:, or #drag messages
	clickState 	<Symbol>	The internal state of handling the last event (#firstClickDown, #firstClickUp, #firstClickTimedOut)
	firstClickDown 	<MorphicEvent>	The #mouseDown event after which the client wished to receive #click: or similar messages
	firstClickUp 	<MorphicEvent>	The first mouse up event which came in before the double click time out was exceeded (it is sent if there is a timout after the first mouse up event occured)
	firstClickTime 	<Integer>	The millisecond clock value of the first event
	clickSelector 	<Symbol>	The selector to use for sending #click: messages
	dblClickSelector 	<Symbol>	The selector to use for sending #doubleClick: messages
	dblClickTime 	<Integer>	Timout in milliseconds for a double click operation
	dragSelector 	<Symbol>	The selector to use for sending #drag: messages
	dragThreshold 	<Integer>	Threshold used for determining if a #drag: message is sent (pixels!)

"
Class {
	#name : #MouseClickState,
	#superclass : #Object,
	#instVars : [
		'clickClient',
		'clickState',
		'firstClickDown',
		'firstClickUp',
		'firstClickTime',
		'clickSelector',
		'dblClickSelector',
		'dblClickTime',
		'dblClickTimeoutSelector',
		'dragSelector',
		'dragThreshold',
		'localStamp'
	],
	#category : #Morphic-Base-ProgressBar
}

{ #category : #'event handling' }
MouseClickState >> checkTimeoutFrom: aHand [
	
	localStamp ifNil: [ localStamp := Time millisecondClockValue. ^ self ].
	
	(Time millisecondClockValue - localStamp ) > dblClickTime ifFalse: [ ^ self ].
	
	clickState == #firstClickDown ifTrue: [ 
		clickState := #firstClickTimedOut.
		dragSelector ifNotNil: [ ^ self ]. 
	].

	aHand resetClickState.
	self doubleClickTimeout.
	
	clickState == #firstClickTimedOut ifTrue:[ self click ]
	



]

{ #category : #'event handling' }
MouseClickState >> click [

	clickSelector ifNotNil: [clickClient perform: clickSelector with: firstClickDown]
]

{ #category : #initialize }
MouseClickState >> client: aMorph click: aClickSelector dblClick: aDblClickSelector dblClickTime: timeOut dblClickTimeout: aDblClickTimeoutSelector drag: aDragSelector threshold: aNumber event: firstClickEvent [
	clickClient := aMorph.
	clickSelector := aClickSelector.
	dblClickSelector := aDblClickSelector.
	dblClickTime := timeOut.
	dblClickTimeoutSelector := aDblClickTimeoutSelector.
	dragSelector := aDragSelector.
	dragThreshold := aNumber.
	firstClickDown := firstClickEvent.
	firstClickTime := firstClickEvent timeStamp.
	clickState := #firstClickDown.
	localStamp := Time millisecondClockValue. 
]

{ #category : #'event handling' }
MouseClickState >> doubleClick [

	dblClickSelector ifNotNil: [clickClient perform: dblClickSelector with: firstClickDown]
]

{ #category : #'event handling' }
MouseClickState >> doubleClickTimeout [

	dblClickTimeoutSelector ifNotNil: [
		clickClient perform: dblClickTimeoutSelector with: firstClickDown]
]

{ #category : #'event handling' }
MouseClickState >> drag: event [

	dragSelector ifNotNil: [clickClient perform: dragSelector with: event]
]

{ #category : #'event handling' }
MouseClickState >> handleEvent: evt from: aHand [
	"Process the given mouse event to detect a click, double-click, or drag.
	Return true if the event should be processed by the sender, false if it shouldn't.
	NOTE: This method heavily relies on getting *all* mouse button events."
	| localEvt timedOut isDrag |
	timedOut := (evt timeStamp - firstClickTime) > dblClickTime.
	localEvt := evt transformedBy: (clickClient transformedFrom: aHand owner).
	isDrag := (localEvt position - firstClickDown position) r > dragThreshold.
	clickState == #firstClickDown ifTrue: [
		"Careful here - if we had a slow cycle we may have a timedOut mouseUp event"
		(timedOut and:[localEvt isMouseUp not]) ifTrue:[
			"timeout before #mouseUp -> keep waiting for drag if requested"
			clickState := #firstClickTimedOut.
			dragSelector ifNil:[
				aHand resetClickState.
				self doubleClickTimeout; click "***"].
			^true].
		localEvt isMouseUp ifTrue:[

			(timedOut or:[dblClickSelector isNil]) ifTrue:[
				self click.
				aHand resetClickState.
				^true].
			"Otherwise transfer to #firstClickUp"
			firstClickUp := evt copy.
			clickState := #firstClickUp.
			"If timedOut or the client's not interested in dbl clicks get outta here"
			self click.
			aHand handleEvent: firstClickUp.
			^false].
		isDrag ifTrue:["drag start"
			self doubleClickTimeout. "***"
			aHand resetClickState.
			dragSelector "If no drag selector send #click instead"
				ifNil: [self click]
				ifNotNil: [self drag: firstClickDown].
			^true].
		^false].

	clickState == #firstClickTimedOut ifTrue:[
		localEvt isMouseUp ifTrue:["neither drag nor double click"
			aHand resetClickState.
			self doubleClickTimeout; click. "***"
			^true].
		isDrag ifTrue:["drag start"
			aHand resetClickState.
			self doubleClickTimeout; drag: firstClickDown. "***"
			^true].
		^false].

	clickState == #firstClickUp ifTrue:[
		(timedOut) ifTrue:[
			"timed out after mouseUp - signal timeout and pass the event"
			aHand resetClickState.
			self doubleClickTimeout. "***"
			^true].
		localEvt isMouseDown ifTrue:["double click"
			clickState := #secondClickDown.
			^false]].

	clickState == #secondClickDown ifTrue: [
		timedOut ifTrue:[
			"timed out after second mouseDown - pass event after signaling timeout"
			aHand resetClickState.
			self doubleClickTimeout. "***"
			^true].
		isDrag ifTrue: ["drag start"
			self doubleClickTimeout. "***"
			aHand resetClickState.
			dragSelector "If no drag selector send #click instead"
				ifNil: [self click]
				ifNotNil: [self drag: firstClickDown].
			^true].
		localEvt isMouseUp ifTrue: ["double click"
			aHand resetClickState.
			self doubleClick.
			^false]
	].

	^true

]

{ #category : #printing }
MouseClickState >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $[; print: clickState; nextPut: $]

]
