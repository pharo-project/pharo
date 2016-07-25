private
checkUntypedVariablesOnly: aString 
	| context model |
	context := self 
		createContextFor: aString
		at: aString size.
	self assert: context isVariablesOnly.
	model := context model.
	model loadEntries.
	self assert: model hasMessage not.
	self assert: (model entriesOfType: #instVar) notEmpty.
	context narrowWith: 'a'.
	self assert: (model entriesOfType: #selector) isEmpty