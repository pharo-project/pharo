updating
updateButton: evt
	"Update the button due to mouse activity in the receiver."

	self enabled ifFalse: [^self].
	evt isMouseDown ifTrue: [
		self buttonMorph state == #on ifTrue: [^self buttonMorph state: #repressed].
		self buttonMorph state == #off ifTrue: [^self buttonMorph state: #pressed]].
	evt isMouseUp ifTrue: [
		self buttonMorph state == #repressed ifTrue: [
			^self buttonMorph state: #off; doButtonAction].
		self buttonMorph state == #pressed ifTrue: [
			^self buttonMorph state: #on; doButtonAction]].
	evt isMove ifTrue: [
		(self containsPoint: evt cursorPoint)
			ifTrue: [self buttonMorph state == #on ifTrue: [^self buttonMorph state: #repressed].
					self buttonMorph state == #off ifTrue: [^self buttonMorph state: #pressed]]
			ifFalse: [self buttonMorph state == #repressed ifTrue: [^self buttonMorph state: #on].
					self buttonMorph state == #pressed ifTrue: [^self buttonMorph state: #off]]]