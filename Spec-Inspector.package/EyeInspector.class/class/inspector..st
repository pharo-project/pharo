inspecting
inspector: anObject 
	^ anObject class inspectorClass new
		inspect: anObject;
		yourself