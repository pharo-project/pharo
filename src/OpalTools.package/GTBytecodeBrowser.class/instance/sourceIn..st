building
sourceIn: a
	^ a pharoMethod 
		smalltalkClass: [ :each | each method methodClass ]; 
		display: [ :each | each sourceCode ]