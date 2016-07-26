instance creation
in: aRBProgramNode named: aName
	aRBProgramNode hasWatchpoint ifTrue: [ ^ aRBProgramNode watchpoint ].
	^self new 
		node: aRBProgramNode;
		tagname: aName;
		install.