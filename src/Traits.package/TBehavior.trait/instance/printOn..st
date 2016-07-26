printing
printOn: aStream 
	"Refer to the comment in Object|printOn:." 

	aStream nextPutAll: 'a descendent of '.
	self superclass printOn: aStream