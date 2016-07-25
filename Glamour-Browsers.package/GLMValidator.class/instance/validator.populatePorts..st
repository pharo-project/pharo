accessing
validator: aString populatePorts: aCollectionOfPorts

	self addButtonAction:   
			((GLMGenericAction new) 
				action: [:tmpBrowser |
					aCollectionOfPorts do: [:aSymbol |
						tmpBrowser explicitlyInnerPortNamed: aSymbol.  
 						]
					]; 
				title: aString; 
				yourself).
				
	