*Tool-Diff
diff
	"Open a diff browser on the changes."

	(DiffMorph
		from: self fromSource
		to: self toSource
		contextClass: (self isClassPatch ifTrue: [nil] ifFalse: [self targetClass]))
			extent: 400@300;
			openInWindowLabeled: 'Diff' translated