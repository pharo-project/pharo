accessing
contents: anObject
 
	controller contents: anObject.
	
	self resetViewTo: controller cursorPosition.
	self controllerChanged.