events
mouseUp: evt
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	evt hand mouseFocus == owner ifFalse: [^self].
	
	self contentString ifNotNil:[
		self contents: self contentString withMarkers: true inverse: true.
		self refreshWorld.
		(Delay forMilliseconds: 200) wait].
	owner rootMenu selectItem: nil event: evt.
	self invokeWithEvent: evt.		
