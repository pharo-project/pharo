inspecting
inspect: anObject 
	InspectorNavigator openInspector: (self inspector: anObject).
	^ anObject