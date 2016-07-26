inspecting
basicInspect: anObject 
	^ self new 
		inspect: anObject;
		openWithSpec: #inspectorSpec;
		yourself