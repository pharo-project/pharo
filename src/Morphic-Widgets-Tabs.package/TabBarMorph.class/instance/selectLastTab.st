protocol
selectLastTab
	
	tabs ifEmpty: [ ^self ].
	tabs last selected: true