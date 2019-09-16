"
Probably to be removed to use metacello toolbox
"
Class {
	#name : #ConfigurationGenerator,
	#superclass : #Object,
	#instVars : [
		'repository',
		'workingCopy'
	],
	#category : #'Spec2-Tools-Monticello'
}

{ #category : #metacello }
ConfigurationGenerator >> addMetacelloBaseline [

	| baselineNumber existingBaselines msg selectorName addedPackages package stream packagePattern |

	"0 -- must have a repository selected"
	repository
		ifNil: [ UIManager default inform: 'Please select a repository and try again'.
			^ self
			].	"1 -- Selecting baseline name"
	existingBaselines := self baseLines.
	msg := existingBaselines size > 0
		ifTrue: [ 'Current baselines:' , String cr
				, ( existingBaselines inject: '' into: [ :sum :el | sum , el , String cr ] )
				, 'Enter a new baseline number'
			]
		ifFalse: [ 'Enter a new baseline number ("-baseline" will be automatically added)' ].
	baselineNumber := UIManager default request: msg initialAnswer: '1.0'.
	baselineNumber ifNil: [ ^ self ].	"2 -- add package names"
	addedPackages := OrderedCollection new.
	packagePattern := UIManager default
		request: 'Please enter a pattern to filter package names'
		initialAnswer: '*'.
	[ | packageNames workingCopies |

	packageNames := OrderedCollection new.
	workingCopies := OrderedCollection new.
	self workingCopies
		do: [ :pkg | 
			( ( packagePattern match: pkg package name )
				and: [ ( addedPackages includes: pkg package name ) not ] )
				ifTrue: [ packageNames add: pkg package name.
					workingCopies add: pkg
					]
			].
	package := UIManager default
		chooseFrom: packageNames
		values: workingCopies
		title: 'Add dependent package (cancel to stop)'.
	package ifNotNil: [ addedPackages add: package package name ]
	] doWhileTrue: [ package notNil ].	"3 -- Creating baseline"
	selectorName := baselineNumber copyWithoutAll: '.-'.
	stream := WriteStream on: String new.
	stream nextPutAll: 'baseline'.
	stream nextPutAll: selectorName.
	stream
		nextPutAll:
			': spec 
        <version: '''.
	stream nextPutAll: baselineNumber.
	stream
		nextPutAll:
			'-baseline''>
        
        spec for: #common do: [
                spec blessing: #baseline.
                spec repository: '''.
	stream nextPutAll: repository description.
	stream
		nextPutAll:
			'''.    
                "spec 
                        package: ''Example-Core'';
                        package: ''Example-Tests'' with: [ spec requires: ''Example-Core'' ]"
'.
	addedPackages
		do: [ :pName | 
			stream
				nextPutAll: '			spec package: ''';
				nextPutAll: pName;
				nextPutAll: '''.';
				nextPutAll: String cr
			].
	stream nextPutAll: '                ].'.
	self configurationClass compile: stream contents classified: 'baselines'
]

{ #category : #metacello }
ConfigurationGenerator >> baseLines [ 
	| existingBaselines |
	existingBaselines := self configurationClass methods select: [:cm | cm selector beginsWith: 'baseline'].
	existingBaselines := (existingBaselines collect: [:m | m pragmas first argumentAt: 1]) asSortedCollection.
	^ existingBaselines
]

{ #category : #metacello }
ConfigurationGenerator >> browseConfiguration [
	self configurationClass browse
]

{ #category : #metacello }
ConfigurationGenerator >> configurationClass [
	^ Smalltalk globals at: workingCopy package name asSymbol 
]

{ #category : #metacello }
ConfigurationGenerator >> createDevelopmentVersion [
	self hasAnyBaseline 
		ifFalse: [ UIManager default inform: 'Please, define a baseline first'. ^ self ].
	^ self createVersionForBlessing: #development
]

{ #category : #metacello }
ConfigurationGenerator >> createReleaseVersion [
	self hasAnyBaseline 
		ifFalse: [ UIManager default inform: 'Please, define a baseline first'. ^ self ].
		
	^ self createVersionForBlessing: #release
]

{ #category : #metacello }
ConfigurationGenerator >> createVersionForBlessing: blessingSelector [
	"blessingSelector = #release or #development"

	| existingVersions msg initialAnswer newVersionNumber selectorName stream mcWorkingCopy |
	existingVersions := self configurationClass methods
		select: [ :cm | cm selector beginsWith: 'version' ].
	existingVersions := (existingVersions
		collect: [ :m | m pragmas first argumentAt: 1 ]) asSortedCollection.
	msg := ''.
	existingVersions size > 4
		ifTrue: [ msg := 'Last 4 versions: '
				,
					(existingVersions
						copyFrom: existingVersions size - 4
						to: existingVersions size) asArray printString , String cr ].
	initialAnswer := existingVersions size > 0
		ifTrue: [ existingVersions last , '.1' ]
		ifFalse: [ '1.0' ].
	newVersionNumber := UIManager default
		request: msg
		initialAnswer: initialAnswer.
	newVersionNumber ifNil: [ ^ self ].
	selectorName := newVersionNumber copyWithoutAll: '.-'.
	stream := WriteStream on: String new.
	stream nextPutAll: 'version'.
	stream nextPutAll: selectorName.
	stream
		nextPutAll:
			': spec 
        <version: '''.
	stream nextPutAll: newVersionNumber.
	stream
		nextPutAll: ''' imports: #(''';
		nextPutAll: self lastBaseLine;
		nextPutAll:
			''')>

        spec for: #common do: [
                spec blessing: ';
		nextPutAll: blessingSelector printString;
		nextPutAll:
			'.
'.
	self dependentPackages
		do: [ :pName | 
			stream
				nextPutAll: '			spec package: ''';
				nextPutAll: pName;
				nextPutAll: ''' with: '''.
			mcWorkingCopy := (MCPackage named: pName) workingCopy.
			mcWorkingCopy needsSaving
				ifTrue: [ self
						inform:
							'The configuration you want to save depends on the package ' , pName
								, '. You first need to save this package in order to create the version'.
					^ self ].
			stream nextPutAll: mcWorkingCopy ancestry ancestors first name.
			stream
				nextPutAll: '''.';
				nextPutAll: String cr ].
	stream nextPutAll: '                ].'.
	self configurationClass
		compile: stream contents
		classified: 'versions'
]

{ #category : #metacello }
ConfigurationGenerator >> dependentPackages [
	"Return the list of dependent packages for the last baseline"

	"This method must be rewritten. It is a shame..."
	
	| lastBaseLineName packageNames |
	lastBaseLineName := self lastBaseLine.

	packageNames := OrderedCollection new.
	(self configurationClass project version: lastBaseLineName)
		record loadDirective packageDirectivesDo: [:directive |
			packageNames add: directive file ].
		
	^ packageNames collect: [:p | 
		(p includes: $.)
			ifTrue: [ (p includes: $-)
						ifTrue: [ (p copyUpToLast: $-) ]
						ifFalse: [ (p copyUpTo: $.) ] ]
			ifFalse: [ p ]
		]

"	lastBaseLineMethod := (self configurationClass methods 
							select: [:cm | cm selector beginsWith: 'baseline'])
								select: [:cm | cm pragmas first arguments first = lastBaseLineName].
	lastBaseLineMethod := lastBaseLineMethod first.
"	
	"This does not work! No idea why!"
"	versionConstructor := (Smalltalk at: #MetacelloVersionConstructor) new.
	self configurationClass new perform: lastBaseLineMethod selector with: versionConstructor.
	versionSpec := (Smalltalk at: #MetacelloVersionSpec) new.
	versionConstructor root: versionSpec
"
	"This is probably the ugliest piece of code I ever wrote. There is really nothing to be proud of."
"	possiblePackageNames := 
		((lastBaseLineMethod literals select: [ :l | l class == ByteString ])
			reject: [:l | l beginsWith: 'http']).
	packageInfos := PackageInfo allPackages select: [ :pi | possiblePackageNames includes: pi packageName ].
"
	"packageInfos now contains the package that the lastest baseline depends on"
	"^ packageInfos collect: #packageName"

	
]

{ #category : #metacello }
ConfigurationGenerator >> hasAnyBaseline [ 
	^ self baseLines notEmpty
]

{ #category : #metacello }
ConfigurationGenerator >> lastBaseLine [
	^ self baseLines last
]
