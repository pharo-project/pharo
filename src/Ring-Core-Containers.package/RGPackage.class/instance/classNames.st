accessing
classNames

	^ self classes isDictionary
		ifTrue: [ self classes keys ]
		ifFalse:[ self classes collect:[ :class|  class name ] ]	