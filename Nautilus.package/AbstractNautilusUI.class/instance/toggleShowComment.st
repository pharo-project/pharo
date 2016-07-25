buttons behavior
toggleShowComment
	self showComment: self showComment not.
	self changed: #commentButtonState
	