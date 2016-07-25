*renraku
check: aClass forCritiquesDo: aCriticBlock
	
	| allSubclasses |
	allSubclasses := aClass withAllSubclasses.
	aClass instVarNames
		select: [ :each | 
			| isRead isWritten |
			isRead := false.
			isWritten := false.
			allSubclasses 
				noneSatisfy: [ :class | 
					isRead ifFalse: [
						isRead := (class whichSelectorsRead: each) isNotEmpty ].
					isWritten ifFalse: [
						isWritten := (class whichSelectorsAssign: each) isNotEmpty ].
					isRead & isWritten ] ]
		thenDo: [ :varName | aCriticBlock cull: (ReTrivialCritique
			withAnchor: (ReSearchStringSourceAnchor
				entity: aClass
				string: varName)
			by: self
			hint: varName) ]