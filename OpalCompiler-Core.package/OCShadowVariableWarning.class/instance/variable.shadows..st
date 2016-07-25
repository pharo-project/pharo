correcting
variable: varNode shadows: semVar
	compilationContext interactive
		ifTrue: [ 
			OCSemanticError new
				node: node;
				compilationContext: compilationContext;
				messageText: self stringMessage;
				signal ]
		ifFalse: [ 
			Transcript
				cr;
				show: self methodClass name;
				show: '>>';
				show: varNode methodNode selector;
				show: '(';
				show: varNode name;
				show: ' is shadowed)']