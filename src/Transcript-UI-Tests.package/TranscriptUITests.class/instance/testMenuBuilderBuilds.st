tests
testMenuBuilderBuilds

	| model menu |
	
	self canRunTest ifFalse: [ ^self skip ].
	
	model := TestableTranscriptRubricOutput new.
	model createTextMorph.
	
	self assert: model textMorph isNotNil.
	
	menu := MenuMorph new.
	
	model addModelItemsToWindowMenu: menu.
	
	[ menu popUpInWorld ] ensure: [ menu delete ].

	self assert: model noModelStuff.
	

	