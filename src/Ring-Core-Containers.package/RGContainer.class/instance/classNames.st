convenient accesses
classNames
	"Retrieves class names (including traits)"
	
	^ self definedClasses isDictionary
		ifTrue: [ self definedClasses keys ]
		ifFalse:[ self definedClasses collect:[ :class|  class name ] ]