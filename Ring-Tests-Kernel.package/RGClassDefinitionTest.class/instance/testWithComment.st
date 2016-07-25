testing
testWithComment
	| newClass newComment |
	
	newClass:= RGClassDefinition named: #Object.
	newComment:= RGCommentDefinition new
						parent: newClass;
						content: 'This is a comment for test';
						stamp: 'VeronicaUquillas 3/22/2011 14:51';
						yourself.
	newClass comment: newComment.

	self assert: (newClass hasComment).
	self assert: (newClass hasStamp).
	self assert: (newClass = newComment parent).
	self assert: (newComment content = 'This is a comment for test').
	self assert: (newComment author = 'VeronicaUquillas').
	self assert: (newComment timeStamp = '3/22/2011 14:51' asDateAndTime).
	
	newClass comment: nil.
	self assert: (newClass hasComment not).
	
	newClass comment:  'This is a comment for test';
			  stamp: 'VeronicaUquillas 3/22/2011 14:51'.
	
	self assert: (newClass comment isRingObject).
