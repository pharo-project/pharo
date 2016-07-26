api
cancel
	
	owner 
		ifNil: [ self delete ] 
		ifNotNil: [ owner delete ]