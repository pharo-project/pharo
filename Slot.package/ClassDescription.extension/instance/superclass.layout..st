*Slot
superclass: aSuperclass layout: aLayout
	layout := aLayout. 
	
	self
		superclass: aSuperclass
		methodDictionary: MethodDictionary new
		format: aLayout format.