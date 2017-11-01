"Added compatibility method: RPackageOrganizer>>addMethod:
This is because Iceberg is meant to be compatible only with Pharo 6.0 and the bootstrap works on Pharo5.0"

! !RPackageOrganizer methodsFor: 'private' stamp: 'CamilloBruni 10/17/2012 11:31'!
addMethod: method

	| rPackage protocol |

	protocol := method protocol ifNil: [ Error signal: 'Should not happen' ].

	rPackage := (self hasPackageForProtocol: protocol inClass: method methodClass)
		ifTrue: [ self packageForProtocol: protocol inClass: method methodClass ]
		ifFalse: [ self registerPackage: (self packageClass named: (protocol copyWithout: $*)) ].

    rPackage ifNotNil: [
		rPackage addMethod: method]! !

[ Metacello new
	baseline: 'PharoBootstrapProcess';
	repository: 'filetree://bootstrap/src';
	load.
] on: Warning do: #resume.

! !TonelRepository methodsFor: 'converting' stamp: 'PabloTesone 01/11/2017 12:00'!
asRing2EnvironmentWith: packageNames

	| aRG2Environment |
	aRG2Environment := RG2Environment new.
	aRG2Environment announcer suspendAllWhile: [ 
		(packageNames) do: [ :pn | 
			| snap defs package version |
			package := aRG2Environment ensurePackageNamed: pn asSymbol.
			version := self versionWithInfo: (self versionInfoFromVersionNamed: pn).
			snap := version snapshot.

			defs := snap definitions.
			defs do: [ :each |			
				each ensureRing2DefinitionIn: aRG2Environment package: package ]
			displayingProgress: 'snap'.] 
		 displayingProgress: 'version'.
		].		
			
	aRG2Environment clean.
	aRG2Environment behaviorsDo: [ :beh |
		beh superclass == beh ifTrue: [ beh superclass: nil. ] ].			
	^ aRG2Environment! 
