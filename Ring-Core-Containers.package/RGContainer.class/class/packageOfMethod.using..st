image package loading
packageOfMethod: aRGMethodDefinition using: packageKeys

	"Looks for the package of aRGMethodDefinition from the image"
	| pName parentPackage |
	(aRGMethodDefinition protocol notNil and:[ aRGMethodDefinition protocol beginsWith: '*' ]) ifFalse:[
		parentPackage := (aRGMethodDefinition parent ifNotNil:[ aRGMethodDefinition parent package ]).	
		^ parentPackage ifNil:[ self packageOfClass: aRGMethodDefinition parent ] ].

	aRGMethodDefinition protocol ifNil: [ ^ nil ].
	pName := (packageKeys 
				detect: [ :each| self category: aRGMethodDefinition protocol asLowercase matches: each value ] ifNone:[ nil ]).
	^ pName ifNotNil:[ RGPackage named: pName key ]