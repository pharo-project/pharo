accessing
sortedKeys

	^ [ [ self object keys sort ] 
		on: Error 
		do: [ "case of non sortable keys" self object keys ]
			] on: Error do: [ #() ]