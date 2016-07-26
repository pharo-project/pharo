running
checkClass: aClass 
	| allSubclasses |
	allSubclasses := aClass withAllSubclasses.
	aClass instVarNames do: [ :each | 
		| isRead isWritten |
		isRead := false.
		isWritten := false.
		allSubclasses 
			detect: [ :class | 
				isRead ifFalse: [ isRead := (class whichSelectorsRead: each) isEmpty not ].
				isWritten ifFalse: [ isWritten := (class whichSelectorsAssign: each) isEmpty not ].
				isRead and: [ isWritten ] ]
			ifNone: [
				result 
					addClass: aClass
					instanceVariable: each ] ]