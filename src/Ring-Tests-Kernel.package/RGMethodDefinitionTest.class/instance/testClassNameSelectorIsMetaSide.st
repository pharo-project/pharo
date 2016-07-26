testing
testClassNameSelectorIsMetaSide
	"self debug: #testClassNameSelectorIsMetaSide"
	
	| rg |
	rg := RGMethodDefinition className: 'Point' selector: #x  isMetaSide: false. 	
	self assert: (rg parentName = 'Point').
	self assert: (rg parent isKindOf:RGClassDefinition).
	self assert: (rg selector = #x).
	self assert: (rg isMetaSide not).