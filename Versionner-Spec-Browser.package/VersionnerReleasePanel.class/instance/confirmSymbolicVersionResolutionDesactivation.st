confirmation
confirmSymbolicVersionResolutionDesactivation
	(UIManager default confirm: 'Are you sure you do not want to resolve symbolic versions? It will lead to unreproductable loads.')
		ifFalse: [ symbolicVersionResolutionCheckBox state: true ]