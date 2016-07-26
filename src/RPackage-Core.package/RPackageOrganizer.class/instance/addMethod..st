private
addMethod: method
	"we have to register the method in the parent RPackage of the class. 
	to do that, we first have to look if the method is an extension from an external package:"

	| rPackage protocol |
	
	protocol := method protocol ifNil: [ Error signal: 'Should not happen' ].
	
	rPackage := (self hasPackageForProtocol: protocol inClass: method methodClass)
		ifTrue: [ self packageForProtocol: protocol inClass: method methodClass ]
		ifFalse: [ self registerPackage: (self packageClass named: (protocol copyWithout: $*)) ].
		
	rPackage addMethod: method