styling
addIconStyle: aNode from: start to: stop
	|rejectClass|
	
	rejectClass := { RBMethodNode . RBSequenceNode }.
	(rejectClass includes: aNode class) ifTrue: [ ^self ]. "do nothing" 
	aNode hasBeenExecuted ifTrue: [  super addIconStyle: aNode from: start to: stop]