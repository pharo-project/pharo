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
	#name : 'MouseClickState',
	#superclass : 'Object',
	#instVars : [
		'clickClient',
		'clickState',
		'firstClickDown',
		'firstClickUp',
		'firstClickTime',
		'firstClickGlobalPosition',
		'clickSelector',
		'dragSelector',
		'dragThreshold',
		'localStamp',
		'dragDoubleClickSelector',
		'doubleClickSelector',
		'doubleClickTime',
		'doubleClickTimeoutSelector'
	],
	#category : 'Morphic-Base-ProgressBar',
	#package : 'Morphic-Base',
	#tag : 'ProgressBar'
}

{ #category : 'instance creation' }
MouseClickState class >> client: aMorph event: aMouseEvent [
	^ self new
		clickClient: aMorph;
		event: aMouseEvent;
		yourself
]

{ #category : 'event handling' }
MouseClickState >> checkTimeoutFrom: aHand [

	localStamp ifNil: [ localStamp := Time millisecondClockValue. ^ self ].

	(Time millisecondClockValue - localStamp ) > doubleClickTime ifFalse: [ ^ self ].

	clickState == #firstClickDown ifTrue: [
		clickState := #firstClickTimedOut.
		dragSelector ifNotNil: [ ^ self ].
	].

	aHand resetClickState.
	self doubleClickTimeout.

	clickState == #firstClickTimedOut ifTrue:[ self click ]
]

{ #category : 'event handling' }
MouseClickState >> click [

	clickSelector ifNotNil: [clickClient perform: clickSelector with: firstClickDown]
]

{ #category : 'accessing' }
MouseClickState >> clickClient [

	^ clickClient
]

{ #category : 'accessing' }
MouseClickState >> clickClient: anObject [

	clickClient := anObject
]

{ #category : 'accessing' }
MouseClickState >> clickSelector [

	^ clickSelector
]

{ #category : 'accessing' }
MouseClickState >> clickSelector: anObject [

	clickSelector := anObject
]

{ #category : 'accessing' }
MouseClickState >> clickState [

	^ clickState
]

{ #category : 'accessing' }
MouseClickState >> clickState: anObject [

	clickState := anObject
]

{ #category : 'initialization' }
MouseClickState >> client: aMorph click: aClickSelector dblClick: aDblClickSelector dblClickTime: timeOut dblClickTimeout: aDblClickTimeoutSelector drag: aDragSelector threshold: aNumber event: firstClickEvent [
	clickClient := aMorph.
	clickSelector := aClickSelector.
	doubleClickSelector := aDblClickSelector.
	doubleClickTime := timeOut.
	doubleClickTimeoutSelector := aDblClickTimeoutSelector.
	dragSelector := aDragSelector.
	dragThreshold := aNumber.
	firstClickDown := firstClickEvent.
	firstClickTime := firstClickEvent timeStamp.
	firstClickGlobalPosition := (aMorph transformedFrom: firstClickEvent hand owner)
		localPointToGlobal: firstClickEvent position.
	clickState := #firstClickDown.
	localStamp := Time millisecondClockValue
]

{ #category : 'event handling' }
MouseClickState >> doubleClick [

	doubleClickSelector ifNotNil: [clickClient perform: doubleClickSelector with: firstClickDown]
]

{ #category : 'accessing' }
MouseClickState >> doubleClickSelector [
	^ doubleClickSelector
]

{ #category : 'accessing' }
MouseClickState >> doubleClickSelector: anObject [

	doubleClickSelector := anObject
]

{ #category : 'accessing' }
MouseClickState >> doubleClickTime [

	^ doubleClickTime
]

{ #category : 'accessing' }
MouseClickState >> doubleClickTime: anObject [

	doubleClickTime := anObject
]

{ #category : 'event handling' }
MouseClickState >> doubleClickTimeout [

	doubleClickTimeoutSelector ifNotNil: [
		clickClient perform: doubleClickTimeoutSelector with: firstClickDown]
]

{ #category : 'accessing' }
MouseClickState >> doubleClickTimeoutSelector [

	^ doubleClickTimeoutSelector
]

{ #category : 'accessing' }
MouseClickState >> doubleClickTimeoutSelector: anObject [

	doubleClickTimeoutSelector := anObject
]

{ #category : 'event handling' }
MouseClickState >> drag: event [

	dragSelector ifNotNil: [
		clickClient perform: dragSelector with: event ].
	WorldMorph executeDragBlockWith: clickClient
]

{ #category : 'event handling' }
MouseClickState >> dragDoubleClick: event [
	dragDoubleClickSelector
		ifNil: [ self drag: event ]
		ifNotNil: [clickClient perform: dragDoubleClickSelector with: event]
]

{ #category : 'accessing' }
MouseClickState >> dragDoubleClickSelector [

	^ dragDoubleClickSelector
]

{ #category : 'accessing' }
MouseClickState >> dragDoubleClickSelector: anObject [

	dragDoubleClickSelector := anObject
]

{ #category : 'accessing' }
MouseClickState >> dragSelector [

	^ dragSelector
]

{ #category : 'accessing' }
MouseClickState >> dragSelector: anObject [

	dragSelector := anObject
]

{ #category : 'accessing' }
MouseClickState >> dragThreshold [

	^ dragThreshold
]

{ #category : 'accessing' }
MouseClickState >> dragThreshold: anObject [

	dragThreshold := anObject
]

{ #category : 'accessing' }
MouseClickState >> event: aMouseEvent [
	self
		firstClickDown: aMouseEvent;
		firstClickTime: aMouseEvent timeStamp;
		firstClickGlobalPosition: ((clickClient transformedFrom: aMouseEvent hand owner)
			localPointToGlobal: aMouseEvent position)
]

{ #category : 'accessing' }
MouseClickState >> firstClickDown [

	^ firstClickDown
]

{ #category : 'accessing' }
MouseClickState >> firstClickDown: anObject [

	firstClickDown := anObject
]

{ #category : 'accessing' }
MouseClickState >> firstClickGlobalPosition [

	^ firstClickGlobalPosition
]

{ #category : 'accessing' }
MouseClickState >> firstClickGlobalPosition: anObject [

	firstClickGlobalPosition := anObject
]

{ #category : 'accessing' }
MouseClickState >> firstClickTime [

	^ firstClickTime
]

{ #category : 'accessing' }
MouseClickState >> firstClickTime: anObject [

	firstClickTime := anObject
]

{ #category : 'accessing' }
MouseClickState >> firstClickUp [

	^ firstClickUp
]

{ #category : 'accessing' }
MouseClickState >> firstClickUp: anObject [

	firstClickUp := anObject
]

{ #category : 'event handling' }
MouseClickState >> handleEvent: evt from: aHand [
	"Process the given mouse event to detect a click, double-click, or drag.
	Return true if the event should be processed by the sender, false if it shouldn't.
	NOTE: This method heavily relies on getting *all* mouse button events."
	| localEvt timedOut isDrag isDragSecond |
	timedOut := (evt timeStamp - firstClickTime) > doubleClickTime.
	localEvt := evt transformedBy: (clickClient transformedFrom: aHand owner).
	isDrag := (evt position - firstClickGlobalPosition) r > dragThreshold.
	isDragSecond := evt position ~= firstClickGlobalPosition.
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

			"If timedOut or the client's not interested in dbl clicks get outta here"
			(timedOut or:[doubleClickSelector isNil]) ifTrue:[
				self click.
				aHand resetClickState.
				^true].

			"Change the state to #firstClickUp.
			Then queue the mouseUp event for later processing.
			We will handle a click now."
			clickState := #firstClickUp.
			firstClickUp := evt copy.
			aHand queuePendingEvent: firstClickUp.

			self click.

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
		(timedOut or: [localEvt isMouseDown and: [
				isDrag or: [localEvt buttons ~= firstClickDown buttons]]]) 
		ifTrue:[
			"timed out after mouseUp, or the mouse has moved, or this click is of a different button.
			Can no longer have a double-click - signal timeout and pass the event"
			aHand resetClickState.
			self doubleClickTimeout. "***"
			^true].
		localEvt isMouseDown ifTrue:["double click"
			clickState := #secondClickDown.
			self doubleClick.
			^false]
	].

	clickState == #secondClickDown ifTrue: [
		isDragSecond ifTrue: ["drag start"
			self doubleClickTimeout. "***"
			aHand resetClickState.
			dragDoubleClickSelector
				ifNotNil: [self dragDoubleClick: firstClickDown].
			^true].
		localEvt isMouseUp ifTrue: ["double click"
			aHand resetClickState.
			^false]
	].

	^true
]

{ #category : 'initialization' }
MouseClickState >> initialize [
	super initialize.
	clickState := #firstClickDown.
	doubleClickTime := HandMorph doubleClickTime.
	localStamp := Time millisecondClockValue.
	dragThreshold := 5
]

{ #category : 'accessing' }
MouseClickState >> localStamp [

	^ localStamp
]

{ #category : 'accessing' }
MouseClickState >> localStamp: anObject [

	localStamp := anObject
]

{ #category : 'printing' }
MouseClickState >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $[; print: clickState; nextPut: $]
]

{ #category : 'accessing' }
MouseClickState >> threshold: aNumber [
	self dragThreshold: aNumber
]
