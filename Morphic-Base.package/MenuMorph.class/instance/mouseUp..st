events
mouseUp: evt
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self fullContainsPoint: evt position) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		evt hand releaseMouseFocus: self.
		^self deleteIfPopUp: evt].
	stayUp ifFalse:[
		"Still in pop-up transition; keep focus"
		evt hand newMouseFocus: self].