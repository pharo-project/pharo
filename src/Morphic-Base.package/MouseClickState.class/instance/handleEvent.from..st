event handling
handleEvent: evt from: aHand
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
