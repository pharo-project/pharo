test inspect
testInspectDictionary
	| inspector |
	inspector :=  self openInspectorOn: Dictionary new.
	inspector changed.
	inspector close.