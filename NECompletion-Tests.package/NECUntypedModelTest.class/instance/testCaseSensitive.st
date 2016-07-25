tests
testCaseSensitive
	| model locals |
	self assert: NECPreferences caseSensitive.
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: OrderedCollection new.
	model loadEntries.
	locals := model entriesOfType: #instVar.
	self assert: locals size == 18.
	self assert: (locals includes: 'third').
	self assert: (locals includes: 'constantInteger').
	self assert: (locals includes: 'complexInit2').
	self assert: (locals includes: 'typeSuggestingParameter').
	model narrowWith: 'fo'