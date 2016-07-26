building
sourceIn: a
	^ a pharoMethod 
		smalltalkClass: [ :each | each method compiledMethod methodClass ]; 
		display: [ :each | each sourceNode source ] 