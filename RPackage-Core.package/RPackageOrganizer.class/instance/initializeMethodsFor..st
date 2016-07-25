initialization-data
initializeMethodsFor: aBehavior
	| package |
	
	package := aBehavior package.  
	(aBehavior organization protocols
		select: [ :each | each isExtensionProtocol not ])
		do: [ :eachProtocol | 
			 (eachProtocol methods 
				select: [ :eachSelector | (aBehavior >> eachSelector) origin = aBehavior ])
				do: [ :eachSelector | package addMethod: (aBehavior >> eachSelector) ] ]