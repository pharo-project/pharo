test slice
testClassesAndExtensionMethodsDo
	"self debug: #testClassesAndExtensionMethodsDo"
	
	| block |

	self assert: (String streamContents: [:stream |
		block := [ :aClassName :selectors |
				stream nextPutAll: aClassName ; nextPutAll: ' '.
				selectors do: [ :selector | stream nextPutAll: selector asString].
				stream cr.].
		{p1 . p2 . p3} do: [ :p |
			p classExtensionSlicesDo: block.
			p metaclassExtensionSlicesDo: block.]]) 
		= 'A2DefinedInP2 methodDefinedInP1
A2DefinedInP2 methodDefinedInP3
A2DefinedInP2 classSideMethodDefinedInP3
'