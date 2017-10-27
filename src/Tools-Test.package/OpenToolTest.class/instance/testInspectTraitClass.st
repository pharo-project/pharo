test inspect
testInspectTraitClass
	| inspector |
	inspector :=  self openInspectorOn: TBehavior.
	inspector changed.
	inspector close.