utils
renameExportedClass: aName to: aNewName on:aMethod
	aMethod literals do:[ :e |
		(e isKindOf: HEAdditionalMethodState) ifTrue:[
			e value value do:[:x | 
				(x method className = aName) ifTrue: [ x method className: aNewName ]					
			].
		]
	].