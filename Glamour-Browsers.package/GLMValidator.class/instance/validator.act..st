accessing
validator: aString act: aBlock 

	"will populate all ports by default at the end"
	self addButtonAction:   
			((GLMGenericAction new) 
				action: [:tmpBrowser |
					
					tmpBrowser panes first ports do: [:aPort |
						tmpBrowser explicitlyInnerPortNamed: aPort name
						].
					aBlock value: tmpBrowser.
					]; 
				title: aString; 
				yourself).
				