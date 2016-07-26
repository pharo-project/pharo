*MonticelloGUI
sourceText
	^ self isClassPatch 
		ifTrue: [(TextDiffBuilder from: self fromSource to: self toSource) buildDisplayPatch]
		ifFalse: [TextDiffBuilder buildDisplayPatchFrom:  self fromSource to: self toSource inClass: self targetClass]
