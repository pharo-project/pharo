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

[ | repositoryPathString |

  repositoryPathString := ((Smalltalk os environment at: 'BOOTSTRAP_REPOSITORY' ifAbsent: ['.']) asFileReference / 'bootstrap' / 'src') fullName.
  Metacello new
	baseline: 'PharoBootstrapProcess';
	repository: 'filetree://', repositoryPathString;
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
	^ aRG2Environment! !
	
'From Pharo6.0 of 13 May 2016 [Latest update: #60508] on 7 December 2017 at 1:00:41.915623 pm'!

!RG2WeakLayoutDefinition methodsFor: '*Ring2-Hermes' stamp: 'PavelKrivanek 12/7/2017 13:00'!
layoutName

	^ #WeakLayout! !

'From Pharo6.0 of 13 May 2016 [Latest update: #60508] on 6 December 2017 at 10:35:49.58245 pm'!

!HERing2ToHermesBuilder methodsFor: 'visiting' stamp: 'PavelKrivanek 12/6/2017 22:34'!
visitClass: aClass
	| aHEClass |
	
	aHEClass := HEClass new.
	self visitMethodContainer: aClass using: aHEClass.

	aHEClass className: aClass name.
	aHEClass superclass: ((aClass superclass)
		ifNil: [ 'nil' ]
		ifNotNil: [ aClass superclass name ]).
	aHEClass instanceVariables: ((aClass slots collect: #name) joinUsing: ' ').
	aHEClass classInstancevariables: ((aClass metaclass slots collect: #name) joinUsing: ' ').
	aHEClass classVariables: (aClass classPool keys joinUsing: ' ').
	aHEClass sharedPools: ((aClass sharedPools collect: #name) joinUsing: ' ').
	aHEClass layoutClass: aClass layout layoutName.

	aHEClass category: aClass category.
	aHEClass traitComposition: aClass traitComposition asExportedLiteral.
	aHEClass classTraitComposition: aClass theMetaClass traitComposition asExportedLiteral.


	^ aHEClass.! !
