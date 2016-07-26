accessing
parseSemanticVersionFor: versionString ifError: aBlock 
	"This is probably not the best way to retrieve a semantic version :P"
	| parsedVersion |
	versionString ifNil: [ ^ aBlock value ].
	
	parsedVersion := (versionString substrings: '.') asArray.
 	parsedVersion size > 3 ifTrue: [ ^ aBlock value ].
	
	parsedVersion do: [ :eachPart | 
		(eachPart allSatisfy: [ :each | each isDigit ])
			ifFalse: [ ^ aBlock value ] ].
		
	[ parsedVersion size < 3 ] 
		whileTrue: [ parsedVersion := parsedVersion copyWith: 0 ].

	^ parsedVersion collect: #asInteger