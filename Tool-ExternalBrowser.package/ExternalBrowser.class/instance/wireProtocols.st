initialize
wireProtocols
	protocols
		whenSelectedItemChanged: [ :protocol | 
			self resetSelectorSelection.
			protocol 
				ifNil: [ selectors items: #() ] 
				ifNotNil: [ selectors items: (self selectorsOfProtocol: protocol) ] ]