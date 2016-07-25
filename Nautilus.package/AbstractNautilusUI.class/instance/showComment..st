accessing
showComment: aBoolean
	
	self okToChangeComment
		ifFalse: [ ^ self ].
	self model showComment: aBoolean.
	
	self updateCodePanel