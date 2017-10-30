test inspect
testInspectArray
	| inspector |
	inspector := self openInspectorOn: (1 to: 1000) asArray.
	inspector changed.
	inspector close.