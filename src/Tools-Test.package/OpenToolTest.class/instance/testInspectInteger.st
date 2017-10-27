test inspect
testInspectInteger
	| inspector |
	inspector :=  self openInspectorOn: 1.
	inspector changed.
	inspector close.