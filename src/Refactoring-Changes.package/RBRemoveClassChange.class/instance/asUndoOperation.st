converting
asUndoOperation
	| classChanges |
	classChanges := RBCompositeRefactoryChange new.
	self changeClass withAllSubclasses do: [ :each | 
		classChanges defineClass: each definition.
		each class instVarNames 
			do: [ :varName | classChanges addInstanceVariable: varName to: each class ].
		each selectors 
			do: [ :selector | classChanges compile: (each sourceCodeAt: selector) in: each ].
		each class selectors 
			do: [ :selector | classChanges compile: (each class sourceCodeAt: selector) in: each class ] ].
	^ classChanges