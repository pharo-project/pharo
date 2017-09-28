validation
check: classSymbol

	self enabled ifFalse: [ ^false ].
	
	(self shouldNotBeRedefined: classSymbol)		
			ifTrue: [ self notify: '"' , classSymbol , '" should not be redefined as its structure is known to the VM. \Only proceed if you know what you are doing!' withCRs ].

