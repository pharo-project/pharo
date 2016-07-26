image package loading
findPackageOfMethod: aRGMethodDefinition using: packageKeys

	"Look for the package of an extension method. nil otherwise"
	| pair lname |	
	self packages isEmpty ifTrue: [ ^nil ].
	(aRGMethodDefinition protocol beginsWith: '*') ifFalse:[ ^ nil ].
	lname := aRGMethodDefinition protocol asLowercase.
 	pair := packageKeys detect:[ :assoc| self class category: lname matches: assoc value ] ifNone:[ nil ].
	^ pair ifNotNil:[ self packages at: pair key ]