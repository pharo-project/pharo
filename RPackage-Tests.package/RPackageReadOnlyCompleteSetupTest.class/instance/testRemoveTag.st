test tag class
testRemoveTag
	"self debug: #testRemoveTag"

	self assert: p1 classTags isEmpty.
	p1 addClassDefinitionName: #A1DefinedInP1 toClassTag: #foo.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 1.
	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #foo.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 2.
	p1 removeClassTag: #bar.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 2.
	p1 removeClassTag: #foo.
	self deny: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 0