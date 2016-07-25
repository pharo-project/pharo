tests - instead
testFailingDoubleRWInsteadSimplified

	|readLink readMO readNode|

	readNode := ((ReflectivityExamples>>#exampleLazyInit) 
		variableReadNodes select: [ :node | node name = 'singleton' ]) first.

	"default MO for rw -- bug manifests on install"
	readMO := [ :object | object class classVarNamed: 'singleton' ].
		
	readLink := MetaLink new 
		metaObject: readMO; 
		selector: #value:;
		control: #instead;
		options: #(+ optionCompileOnLinkInstallation);
		arguments: #(object).
			
	readNode link: readLink.
	readLink uninstall.
