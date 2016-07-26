accessing
defaultAction
	familyName ifNil: [ familyName := 'NoName' ].
	pixelSize ifNil: [ pixelSize := 12 ].

	^((familyName beginsWith: 'Comic')
		ifTrue: [ TextStyle named: (StandardFonts buttonFont familyName) ]
		ifFalse: [ TextStyle default ]) fontOfSize: pixelSize.