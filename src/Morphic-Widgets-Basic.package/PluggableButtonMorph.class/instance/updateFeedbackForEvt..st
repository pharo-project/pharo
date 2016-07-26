updating
updateFeedbackForEvt: evt

	| newState |
	newState := self containsPoint: evt cursorPoint.
	newState = showSelectionFeedback ifFalse: [
		self showSelectionFeedback: newState.
		self changed; layoutChanged].
