private
createAccessorsFor: varName in: aClass

	self createGetter: varName in: aClass.	
	self createSetter: varName in: aClass .
	
	