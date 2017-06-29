converting
asUndoOperation
	| classChanges |
	classChanges := RBRefactoryChangeManager changeFactory compositeRefactoryChange.
	self changeClass withAllSubclasses do: [ :each | 
		classChanges defineClass: each definition.
		each class instVarNames 
			do: [ :varName | classChanges addInstanceVariable: varName to: each class ].
		each selectors 
			do: [ :selector | classChanges compile: (each sourceCodeAt: selector) in: each ].
		each class selectors 
			do: [ :selector | classChanges compile: (each class sourceCodeAt: selector) in: each class ] ].
	^ classChanges