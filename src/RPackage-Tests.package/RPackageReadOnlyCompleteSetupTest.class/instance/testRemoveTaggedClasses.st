test tag class
testRemoveTaggedClasses
	"self debug: #testRemoveTaggedClasses"

	p1 addClassDefinitionName: #A1DefinedInP1 toClassTag: #foo.
	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #foo.
	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #zork.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 2.
	self deny: (((p1 classesForClassTag: #zork) collect: #name) includes: #A1DefinedInP1).
	self assert: (((p1 classesForClassTag: #zork) collect: #name) includes: #B1DefinedInP1).	"now when we remove a class"	"from an existing tags list"
	p1 removeClassDefinitionName: #A1DefinedInP1 fromClassTag: #foo.
	self deny: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 1.	"from a nonexisting tag list"
	p1 removeClassDefinitionName: #B1DefinedInP1 fromClassTag: #taz.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (((p1 classesForClassTag: #zork) collect: #name) includes: #B1DefinedInP1).	"with a class not registered to a tag list"
	p1 removeClassDefinitionName: #Tagada fromClassTag: #foo.
	self deny: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 1