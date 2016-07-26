evaluating
applyTo: aVariablesMapping 
	
	variables do: [:link |
		aVariablesMapping 
			map: link key 
			to: link value ]
	