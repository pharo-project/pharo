building
sourceIn: a
	^ a pharoMethod 
		smalltalkClass: [ :each | each parents first methodClass ]; 
		display: [ :each | each source ]