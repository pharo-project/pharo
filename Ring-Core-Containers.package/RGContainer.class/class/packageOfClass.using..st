image package loading
packageOfClass: aRGBehaviorDefinition using: packageNames

	"Looks for the package of aRGBehaviorDefinition from the image"
	| pName |
	aRGBehaviorDefinition ifNil:[ ^nil ].
	pName := (packageNames 
				detect: [ :each| each = aRGBehaviorDefinition category ]
				ifNone: [ packageNames detect:[ :each| self category: aRGBehaviorDefinition category matches: each ] ifNone:[ nil ] ]).
	^ pName ifNotNil:[ RGPackage named: pName ]