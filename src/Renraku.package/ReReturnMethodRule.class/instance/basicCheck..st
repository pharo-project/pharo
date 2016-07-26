running
basicCheck:	aMethod
	
	^ (aMethod overriddenMethods anySatisfy: [ :method |
		method hasPragmaNamed: #shouldReturn. ]) and: [ 
	aMethod ast lastIsReturn not ]