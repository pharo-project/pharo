actions
preload: loader package: packageSpec

	'Starting preload action' logCr.
	
	Author fullName: self class name.

	"because of epicea deprecation warning"
	Object compile: 'name
	^ self printString'.
	
	Object subclass: #ReAbstractRule
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Renraku-Rules'.
	
