test slice
testClassNamesAndExtensionSelectorsDo2
	"self debug: #testClassNamesAndExtensionSelectorsDo2"
	

	self assert: (String streamContents: [:stream |
		{p1 . p2 .p3} do: [:p |
			p classNamesAndExtensionSelectorsDo: [:aClassName :selector |
				stream nextPutAll: aClassName  ; nextPutAll: ' ' ; nextPutAll:  selector asString; cr ]]])
		= 'A2DefinedInP2 methodDefinedInP1
A2DefinedInP2 methodDefinedInP3
A2DefinedInP2_class classSideMethodDefinedInP3
'.
