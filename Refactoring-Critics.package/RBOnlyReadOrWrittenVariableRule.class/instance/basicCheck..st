running
basicCheck: aClass 
	| allSubclasses |
	allSubclasses := aClass withAllSubclasses.
	^ aClass instVarNames anySatisfy: [ :each | 
		| isRead isWritten |
		isRead := false.
		isWritten := false.
		allSubclasses 
			noneSatisfy: [ :class | 
				isRead ifFalse: [ isRead := (class whichSelectorsRead: each) isNotEmpty ].
				isWritten ifFalse: [ isWritten := (class whichSelectorsAssign: each) isNotEmpty ].
				isRead and: [ isWritten ] ] ]