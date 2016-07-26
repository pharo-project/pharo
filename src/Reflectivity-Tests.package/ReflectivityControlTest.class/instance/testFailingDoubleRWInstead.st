tests - instead
testFailingDoubleRWInstead

	|readLink readMO writeLink writeMO readSites writeSites|

	readSites := (ReflectivityExamples>>#exampleLazyInit) 
		variableReadNodes select: [ :node | node name = 'singleton' ].
		
	writeSites :=  (ReflectivityExamples>>#exampleLazyInit) 
		variableWriteNodes select: [ :node | node name = 'singleton' ].
	
	"default MO for rw -- bug manifests on install"
	readMO := [ :object | object class classVarNamed: 'singleton' ].
	writeMO := [ :object :newValue | object class classVarNamed: 'singleton' put: newValue ].
		
	readLink := MetaLink new 
		metaObject: readMO; 
		selector: #value:;
		control: #instead;
		options: #(+ optionCompileOnLinkInstallation);
		arguments: #(object).
		
	writeLink := MetaLink new 
		metaObject: writeMO; 
		selector: #value:value:;
		control: #instead;
		options: #(+ optionCompileOnLinkInstallation);
		arguments: #(newValue object).
	
	readSites do: [ :readNode | readNode link: readLink ].
	writeSites do: [ :writeNode | writeNode link: writeLink ].
	readLink uninstall.
	writeLink uninstall.