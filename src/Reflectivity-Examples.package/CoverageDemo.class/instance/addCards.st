adding
addCards

	compiledMethods do:[:method|
		codePanelMorph widget addMorph: 
			(ReflectivityCodeMorph compiledMethod: method)
	]
	