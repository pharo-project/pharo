compiled method handling
removeMethod: aCompiledMethod 
	"Remove the method to the receiver as a defined method."
	|  methodClass |
	
	methodClass := aCompiledMethod methodClass. 
	methodClass isMeta
		ifTrue: [
			self 
				removeSelector: aCompiledMethod selector 
				ofMetaclassName: methodClass theNonMetaClass originalName ]
		ifFalse: [
			self 
				removeSelector: aCompiledMethod selector 
				ofClassName: methodClass originalName ].

	^ aCompiledMethod.