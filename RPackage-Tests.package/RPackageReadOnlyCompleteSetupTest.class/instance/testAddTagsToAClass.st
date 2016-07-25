test tag class
testAddTagsToAClass
	"self debug: #testAddTagsToAClass"
	
	self assert: p1 classTags isEmpty.
	p1 addClassDefinitionName: #A1DefinedInP1 toClassTag: #foo.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: ((p1 classesForClassTag: #foo) size = 1).
	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #foo.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: ((p1 classesForClassTag: #foo) size = 2).

	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #zork.
	self assert: (((p1 classesForClassTag: #zork) collect: #name) includes: #B1DefinedInP1).
	self assert: ((p1 classesForClassTag: #foo) size = 2).
	self assert: ((p1 classesForClassTag: #zork) size = 1).