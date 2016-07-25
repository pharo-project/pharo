building
compose  
	"self title: [:each | GTInspector new printObjectAsAnItem: each receiver ]."
	"self morphicAct: [:anotherComposite | anotherComposite entity receiver browse]
				icon: GLMUIThemeExtraIcons glamorousBrowse 
				entitled: 'Browse'."
	self tabulator with: [:browser | 
		browser row: #variables.
		browser transmit to: #variables; andShow: [:a | 
			self variablesIn: a ].
		browser transmit 
			from: #variables port: #entity; 
			to: #variables port: #selection; 
			when: [ :aContext | self debugger selectedVariableName notNil and: [aContext notNil] ];
			transformed: [ :aContext | self selectedVariableForContext: aContext ].
		browser transmit from: #variables; toOutsidePort: #selection; transformed: [ :pair | pair value ] ]