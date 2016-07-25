testing
testCommentWithoutAuthor
	| newComment |
	
	newComment:= RGCommentDefinition new
						content: 'This is a comment for test';
						stamp: '3/22/2011 14:51';
						yourself.
	
	self assert: (newComment hasAuthor not).