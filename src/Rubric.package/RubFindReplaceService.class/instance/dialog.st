user-interface
dialog
	^ dialog ifNil: [ dialog := self newDialog ]
	