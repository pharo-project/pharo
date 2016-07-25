inspecting
inspect: anObject label: aString
	(InspectorNavigator openInspector: (self inspector: anObject)) window title: aString.
	^ anObject
