compiled method handling
removeMethods: aCollection 
	aCollection do: [ :each | self removeMethod: each ]
	