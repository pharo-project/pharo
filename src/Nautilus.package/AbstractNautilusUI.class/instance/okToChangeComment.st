source code area
okToChangeComment

	| answer |
	classCommentTextModel canDiscardEdits ifTrue: [^ true].
	answer :=  self promptForCancel.
	answer ifNil: [ ^ classCommentTextModel canDiscardEdits ];
			ifTrue: [ self acceptCommentFromView  ]
			ifFalse: [  classCommentTextModel cancelEdits  ].	
	^ classCommentTextModel canDiscardEdits
