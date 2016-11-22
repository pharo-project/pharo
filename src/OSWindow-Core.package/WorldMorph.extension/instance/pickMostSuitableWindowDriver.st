*OSWindow-Core
pickMostSuitableWindowDriver
	"TODO: check for headless mode"
	| driver |
	
	driver := OSWindowDriver current.
	
	"well, lets try using the VM driver"
	(driver isNullDriver and: [VMWindowDriver isSupported]) ifTrue: [ 
		^ VMWindowDriver new ].
	
	^ driver