event handling
mouseUp: evt
	"Perform the button action if the mouse pointer is in a button in the group.
	Optimised feedback updates."
	
	|all|
	all := allButtons copy.
	all ifNotNil: [all do: [:m |
		m showSelectionFeedback ifTrue: [
			m showSelectionFeedback: false; changed; layoutChanged]]].
	all ifNil: [^ self].
	allButtons := nil.
	all do: [:m |
		(m containsPoint: evt cursorPoint) ifTrue: [m enabled ifTrue: [ m performAction: evt ]]].
	self showSelectionFeedback ifTrue: [self changed]
