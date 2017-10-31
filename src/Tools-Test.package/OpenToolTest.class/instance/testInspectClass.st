test inspect
testInspectClass
	| inspector |
	inspector :=  self openInspectorOn: Object.
	inspector changed.
	inspector close.