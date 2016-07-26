actions
invokePrintItIn: editorMorph 
	| myeditor result |
	myeditor := editorMorph controller.

	myeditor selection isEmpty ifTrue: [  
		myeditor moveToLineStart; extendSelectionToLineEnd. 
	].

	myeditor selection isEmpty ifFalse: [ 
		result := ' ' , (Smalltalk compiler evaluate: myeditor selection asString) printString.
		myeditor collapseSelection.
		editorMorph insert: result
		
	 ].