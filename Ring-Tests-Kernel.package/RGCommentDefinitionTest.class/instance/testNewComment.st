testing
testNewComment
	| newComment |
	
	newComment:= RGCommentDefinition new
						content: 'This is a comment for test';
						stamp: 'VeronicaUquillas 3/22/2011 14:51';
						yourself.
	
	self assert: newComment isComment.
	self assert: newComment isPassive.
	self assert: newComment name = #Comment.
	self assert: newComment hasStamp.
	self assert: newComment hasAuthor.
	self assert: newComment timeStamp notNil.
	
	self assert: (newComment author = 'VeronicaUquillas').
	self assert: (newComment timeStamp = '3/22/2011 14:51' asDateAndTime).
	self assert: (newComment parent = nil).
	self assert: (newComment environment = Smalltalk globals).