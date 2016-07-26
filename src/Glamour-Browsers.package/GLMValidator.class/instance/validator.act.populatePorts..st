accessing
validator: aString act: aBlock populatePorts: aCollectionOfPorts

	self addButtonAction:   
			((GLMGenericAction new) 
				action: [:tmpBrowser |
					
					aCollectionOfPorts do: [:aSymbol |
						tmpBrowser explicitlyInnerPortNamed: aSymbol.  
 						].
					aBlock value: tmpBrowser.
					]; 
				title: aString; 
				yourself).
				
	