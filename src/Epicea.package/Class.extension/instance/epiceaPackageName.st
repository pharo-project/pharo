*Epicea-Ring
epiceaPackageName
	"Answer my package name for Epicea serialization"

	self flag: #fix.
	^ (RPackage organizer packageOfClassNamed: self name) 
		ifNotNil: [ :package | package name ]
		ifNil: [ 'nil' ]