private
methodForClassNamed: aClassOrClassName selector: aSelector
	^ self classNamed: aClassOrClassName 
		do: [ :class | self methodForClass: class selector: aSelector ] 
		ifNone: [ GTExampleProviderMethod new 
						providerName: aClassOrClassName;
						selector: aSelector asSymbol;
						yourself ]