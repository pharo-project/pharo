test inspect
testInspectArray
	| inspector |
	inspector := (1 to: 1000) asArray inspector.
	inspector changed.
	inspector close.