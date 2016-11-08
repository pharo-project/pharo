source code area
okToChangeComment

	| answer |
	commentTextModel canDiscardEdits ifTrue: [^ true].
	answer :=  self promptForCancel.
	answer ifNil: [ ^ commentTextModel canDiscardEdits ];
			ifTrue: [ self acceptCommentFromView  ]
			ifFalse: [  commentTextModel cancelEdits  ].	
	^ commentTextModel canDiscardEdits
