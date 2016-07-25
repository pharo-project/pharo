actions
invokeInspectItIn: editorMorph 
	| myeditor |
	myeditor := editorMorph controller.

	myeditor selection isEmpty ifTrue: [  
		myeditor moveToLineStart; extendSelectionToLineEnd. 
	].

	myeditor selection isEmpty ifFalse: [ 
		(Smalltalk compiler evaluate: myeditor selection asString) inspect.	
	 ].