Class {
	#name : #MetacelloAbstractVersionConstructor,
	#superclass : #MetacelloAbstractConstructor,
	#instVars : [
		'root',
		'configuration',
		'project',
		'attributeMap',
		'attributeOrder',
		'symbolicVersion',
		'currentContext'
	],
	#category : #'Metacello-Core-Constructors'
}

{ #category : #'method generation' }
MetacelloAbstractVersionConstructor class >> symbolicMethodSelectorAndPragma: selector symbolicVersionSymbol: symbolicVersionSymbol on: strm [
	strm
		nextPutAll: selector asString , ' spec';
		cr;
		tab;
		nextPutAll: '<symbolicVersion: #' , symbolicVersionSymbol asString printString;
		nextPutAll: '>';
		cr
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> addAttribute: anAttribute [

	self attributeOrder add: anAttribute
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> attributeMap [

	attributeMap == nil ifTrue: [ attributeMap := Dictionary new ].
	^attributeMap
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> attributeOrder [

	attributeOrder == nil ifTrue: [ attributeOrder := OrderedCollection new ].
	^attributeOrder
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> author: aBlockOrString [
    "Define author field of version spec (MetacelloMCVersionSpec).
	 If <aBlockOrString> is a String, the version spec author is set to the String.
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the author spec (MetacelloValueHolderSpec). Not Recommended!
	
		spec author: 'dkh'.
		
		spec author: [
			spec value: 'dkh'. ].
	 "

    self root author: aBlockOrString constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> authorForVersion: aBlockOrString [
    aBlockOrString setAuthorInMetacelloConfig: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> baseline: aString [
    self root baseline: aString constructor: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> baseline: aString with: aBlockOrString [
    self root baseline: aString with: aBlockOrString constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> baselineForVersion: aString [
    self setBaseline: aString
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> baselineForVersion: aString with: aBlock [
    aBlock setBaseline: aString withInMetacelloConfig: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> blessing: aBlockOrString [
    "Define blessing field of version spec (MetacelloMCVersionSpec).
	 If <aBlockOrString> is a String, the version spec blessing is set to the String. It is recommended to use a Symbol.
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the blessing spec (MetacelloValueHolderSpec). Not Recommended!
	
		spec blessing: #release.
		
		spec blessing: [
			spec value: #release. ].
	
	The blessing should typically be set to one of three values:
		#baseline - indicating that the version spec is specifying a baseline version
		#development - indicating that the version spec is not stabilized and will change over time
		#release - indicating that the version spec has stabilized and will NOT change over time
	 "

    self root blessing: aBlockOrString constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> blessingForVersion: aBlockOrString [
    aBlockOrString setBlessingInMetacelloConfig: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> className: aString [
    "Define className field of a project spec (MetacelloMCProjectSpec).
	
		spec project: 'CoolBrowser' with: [
			spec className: 'ConfigurationOfCoolBrowser'. ].

	The className field is OPTIONAL in the project spec. If omitted, the className will be created by prepending 'ConfigurationOf' to the project name.
	 "

    self root className: aString constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> classNameForProject: aString [
    self root className: aString
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> configuration [

	^configuration
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> configuration: aConfig [

	configuration := aConfig
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> configuration: aString with: aBlockOrString [
    self root configuration: aString with: aBlockOrString constructor: self
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> configurationClass [

	^self configuration class
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> configurationForVersion: aString with: aBlock [
    aBlock setConfiguration: aString withInMetacelloConfig: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> description: aBlockOrString [
    "Define description field of version spec (MetacelloMCVersionSpec).
	 If <aBlockOrString> is a String, the version spec blessing is set to the String. It is recommended to use a Symbol.
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the blessing spec (MetacelloValueHolderSpec). Not Recommended!
	
		spec description: 'Descriptive comment'.
		
		spec description: [
			spec value: 'Descriptive comment'.
	 "

    self root description: aBlockOrString constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> descriptionForVersion: aBlockOrString [
    aBlockOrString setDescriptionInMetacelloConfig: self
]

{ #category : #private }
MetacelloAbstractVersionConstructor >> evaluatePragma: pragma [
	currentContext := pragma.
	[ self configuration 
		perform: (MetacelloPlatform current selectorForPragma: pragma) 
		with: self ] 
	ensure: [ currentContext := nil ]
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> file: aString [
    "Define file field of a package spec (MetacelloPackageSpec) or project spec (MetacelloMCProjectSpec).

	For a package spec, the file: field is optional in a baseline. In a baseline, the file field may be used to specify a package branch for the package:
	
		spec package: 'MyPackage' with: [
			spec file: 'MyPackage.gemstone'. ]'.

	The file: field is required in a version. In a version, the file field defines the explicit version of the package to be loaded:
	
		spec package: 'MyPackage' with: [
			spec file: 'MyPackage.gemstone-dkh.1'. ]'.

	The following may be used as a short cut for specifying the file field in a version:

		spec package: 'MyPackage' with: 'MyPackage.gemstone-dkh.1'.

	For a project spec, the file field specifies the name of the Monticello package that contains the configuration. If you are using the convention of 
	naming the class and package usingthe  'ConfigurationOf' prefix, then there is no need to specify the file field:
	
		spec project: 'MyProject' with: [
			spec file: 'ConfigurationMyProject'.

	It should only be used when the package name for the configuration is different from the name of the project:

		spec project: 'MyProject' with: [
			spec file: 'MyProject-Metacello'.
	 "

    self root file: aString constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> fileForPackage: aString [
    self root file: aString
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> fileForProject: aString [
    self root file: aString
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> for: attributeListOrSymbol do: aBlock [
    "conditional version support"

    attributeListOrSymbol setForDo: aBlock withInMetacelloConfig: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> for: attributeListOrSymbol version: aString [
    "conditional symbolicVersion support"

    attributeListOrSymbol setForVersion: aString withInMetacelloConfig: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> group: aString overrides: aStringOrCollection [
    self root group: aString overrides: aStringOrCollection constructor: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> group: aString with: aStringOrCollection [
    self root group: aString with: aStringOrCollection constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> groupForVersion: aString overrides: aStringOrCollection [
    | spec |
    spec := self project groupSpec
        name: aString;
        includes: aStringOrCollection;
        yourself.
    self root packages add: spec
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> groupForVersion: aString with: aStringOrCollection [
    | spec |
    spec := self project groupSpec
        name: aString;
        includes: aStringOrCollection;
        yourself.
    self root packages merge: spec
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> import: aStringOrCollection [
  "import names defined in baseline <aString> to be used when loading the version

		spec baseline: 'Sample' with: [ spec repository: 'github://dalehenrich/sample:master/repository' ].
		spec import: 'Sample' ]
	
	or a list of names for multiple project imports:
	
		spec baseline: 'Sample' with: [ spec repository: 'github://dalehenrich/sample:master/repository' ].
		spec baseline: 'Example' with: [ spec repository: 'github://dalehenrich/example:master/repository' ].
		spec import: #('Sample' 'Example')]	
	 "

  self root import: aStringOrCollection constructor: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> import: aString provides: aCollection [
  "import names defined in baseline <aString> to be used when loading the version

	
		spec baseline: 'Sample' with: [ spec repository: 'github://dalehenrich/sample:master/repository' ].
		spec import: 'Sample' provides: #('Sample Core');
		spec baseline: 'Example' with: [ spec repository: 'github://dalehenrich/example:master/repository' ].
		spec import: 'Example' provides: #('Example Core')]	
	 "

  self root import: aString provides: aCollection constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> importForVersion: aString [
    self root import: aString
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> importForVersion: aString provides: anArray [
  self root import: aString provides: anArray
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> includes: anObject [
    self root includes: anObject constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> includesForPackage: anObject [
    self root includes: anObject
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> loads: aStringOrCollection [
  self root loads: aStringOrCollection constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> loadsForProject: anObject [
    self root loads: anObject
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> name: anObject [
    self root name: anObject constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> nameForProject: aString [
    self root name: aString
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> operator: anObject [
    self root operator: anObject constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> operatorForProject: anObject [
    self root operator: anObject
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> package: aString [
    self root package: aString constructor: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> package: aString overrides: aBlock [
    self root package: aString overrides: aBlock constructor: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> package: packageName with: aBlockOrString [
    "Define specification for package <packageName>.
	 If <aBlockOrString> is a String (or Symbol), the String is expected to be a version (or symbolic version).
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the project:
	
		spec package: 'MyPackage' with: '1.0'.
		
		spec package: 'MyPackage' with: [
			spec file:'MyPackage-dkh.1'.
			spec repository: '/opt/gemstone/repository'.
	 "

    self root package: packageName with: aBlockOrString constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> packageForVersion: aString [
    | spec |
    spec := self project packageSpec
        name: aString;
        yourself.
    self root packages add: spec
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> packageForVersion: aString overrides: aBlock [
    | spec |
    spec := self project packageSpec
        name: aString;
        yourself.
    self root packages add: spec.
    self with: spec during: aBlock
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> packageForVersion: packageName with: aBlockOrString [
    aBlockOrString setPackage: packageName withInMetacelloConfig: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> postLoadDoIt: aSymbol [
    self root postLoadDoIt: aSymbol constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> postLoadDoItForPackage: aSymbol [
    self postLoadDoItForSpec: aSymbol
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> postLoadDoItForProject: aSymbol [
    self postLoadDoItForSpec: aSymbol
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> postLoadDoItForSpec: aSymbol [
    self validateDoItSelector: aSymbol.
    self root postLoadDoIt: aSymbol
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> postLoadDoItForVersion: aSymbol [
    self postLoadDoItForSpec: aSymbol
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> preLoadDoIt: aSymbol [
    self root preLoadDoIt: aSymbol constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> preLoadDoItForPackage: aSymbol [
    self preLoadDoItForSpec: aSymbol
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> preLoadDoItForProject: aSymbol [
    self preLoadDoItForSpec: aSymbol
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> preLoadDoItForSpec: aSymbol [
    self validateDoItSelector: aSymbol.
    self root preLoadDoIt: aSymbol
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> preLoadDoItForVersion: aSymbol [
    self preLoadDoItForSpec: aSymbol
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> project [
    project == nil
        ifTrue: [ project := self projectClass new ].
    ^ project
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> project: aString [
    self root project: aString constructor: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> project: aString copyFrom: oldSpecName with: aBlock [
    self root
        project: aString
        copyFrom: oldSpecName
        with: aBlock
        constructor: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> project: aString overrides: aBlock [
    self root project: aString overrides: aBlock constructor: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> project: aString with: aBlockOrString [
    self root project: aString with: aBlockOrString constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> projectForVersion: aString [
    self project: aString with: ''
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> projectForVersion: aString copyFrom: oldSpecName with: aBlock [
    | spec projectSpec |
    projectSpec := self project projectSpec
        name: aString;
        yourself.
    spec := self project projectReferenceSpec
        name: aString;
        projectReference: projectSpec;
        yourself.
    self root packages copy: oldSpecName to: spec.
    self with: projectSpec during: aBlock
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> projectForVersion: aString overrides: aBlock [
    | spec projectSpec |
    projectSpec := self project projectSpec
        name: aString;
        yourself.
    spec := self project projectReferenceSpec
        name: aString;
        projectReference: projectSpec;
        yourself.
    self root packages add: spec.
    self with: projectSpec during: aBlock
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> projectForVersion: aString with: aBlockOrString [
    aBlockOrString setProject: aString withInMetacelloConfig: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> projectPackage: aBlock [
    "projectPackage spec data folded into project spec"

    self root projectPackage: aBlock constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> projectPackageForProject: aBlock [
    self with: self root during: aBlock
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> removeGroup: aString [
    self root removeGroup: aString constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> removeGroupForVersion: aString [
    | spec |
    spec := self project groupSpec
        name: aString;
        yourself.
    self root packages remove: spec
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> removePackage: aString [
    self root removePackage: aString constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> removePackageForVersion: aString [
    | spec |
    spec := self project packageSpec
        name: aString;
        yourself.
    self root packages remove: spec
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> removeProject: aString [
    self root removeProject: aString constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> removeProjectForVersion: aString [
    | spec |
    spec := self project projectReferenceSpec
        name: aString;
        yourself.
    self root packages remove: spec
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> repositories: aBlock [
    self root repositories: aBlock constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoriesForPackage: aBlock [
    self repositoriesForSpec: aBlock
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoriesForProject: aBlock [
    self repositoriesForSpec: aBlock
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoriesForSpec: aBlock [
    self with: self root repositories during: aBlock
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoriesForVersion: aBlock [
    self repositoriesForSpec: aBlock
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> repository: anObject [
    self root repository: anObject constructor: self
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> repository: description username: username password: password [
    self root
        repository: description
        username: username
        password: password
        constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoryForPackage: anObject [
    self repositoryForSpec: anObject
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoryForPackage: aString username: username password: password [
    self repositoryForSpec: aString username: username password: password
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoryForProject: anObject [
    self repositoryForSpec: anObject
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoryForProject: aString username: username password: password [
    self repositoryForSpec: aString username: username password: password
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoryForRepositories: anObject [
    self repositoryForSpec: anObject
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoryForRepositories: aString username: username password: password [
    self repositoryForVersion: aString username: username password: password
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoryForSpec: anObject [
    self root repository: anObject
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoryForSpec: aString username: username password: password [
    self root repository: aString username: username password: password
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoryForVersion: anObject [
    self repositoryForSpec: anObject
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> repositoryForVersion: aString username: username password: password [
    self repositoryForSpec: aString username: username password: password
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> requires: anObject [
    self root requires: anObject constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> requiresForPackage: anObject [
    self root requires: anObject
]

{ #category : #initialization }
MetacelloAbstractVersionConstructor >> reset [
    attributeMap := attributeOrder := nil
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> root [

	^root
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> root: aMetacelloSpec [
    root := aMetacelloSpec
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setAuthorWithBlock: aBlock [

	| spec |
	(spec := self root getAuthor) == nil
		ifTrue: [ 
			spec := self project valueHolderSpec.
			self root setAuthor: spec ].
	self with: spec during: aBlock
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setAuthorWithString: aString [

	self root author: aString
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setBaseline: aString [
    | spec projectSpec |
    projectSpec := self project baselineOfProjectSpec
        name: aString;
        className: 'BaselineOf' , aString;
        yourself.
    spec := self project projectReferenceSpec
        name: aString;
        projectReference: projectSpec;
        yourself.
    self root packages merge: spec.
    ^ projectSpec
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setBaseline: aString withBlock: aBlock [
    | projectSpec |
    projectSpec := self setBaseline: aString.
    self with: projectSpec during: aBlock
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setBlessingWithBlock: aBlock [

	| spec |
	(spec := self root getBlessing) == nil
		ifTrue: [ 
			spec := self project valueHolderSpec.
			self root setBlessing: spec ].
	self with: spec during: aBlock
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setBlessingWithString: aString [

	self root blessing: aString
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setConfiguration: aString withBlock: aBlock [
    | spec projectSpec |
    projectSpec := self project configurationOfProjectSpec
        name: aString;
        yourself.
    spec := self project projectReferenceSpec
        name: aString;
        projectReference: projectSpec;
        yourself.
    self root packages merge: spec.
    self with: projectSpec during: aBlock
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setDescriptionWithBlock: aBlock [

	| spec |
	(spec := self root getDescription) == nil
		ifTrue: [ 
			spec := self project valueHolderSpec.
			self root setDescription: spec ].
	self with: spec during: aBlock
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setDescriptionWithString: aString [

	self root description: aString
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setFor: attributeList do: aBlock [
    "conditional version support"

    attributeList asMetacelloAttributeList
        do: [ :attribute | 
            | blockList |
            blockList := self attributeMap
                at: attribute
                ifAbsent: [ self attributeMap at: attribute put: (blockList := OrderedCollection new) ].
            blockList add: aBlock.
            self addAttribute: attribute ]
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setFor: attributeList version: aString [
    "conditional symbolicVersion support"

    attributeList asMetacelloAttributeList
        do: [ :attribute | 
            self attributeMap at: attribute put: aString.
            self addAttribute: attribute ]
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setPackage: aString withBlock: aBlock [

	| spec |
	spec := 
		(self project packageSpec)
			name: aString;
			yourself.
	self root packages merge: spec.
	self with: spec during: aBlock
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setPackage: aString withString: aFile [

	| spec |
	spec := 
		(self project packageSpec)
			name: aString;
			file: aFile;
			yourself.
	self root packages merge: spec.
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> setProject: aProject [

	project := aProject
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setProject: aString withBlock: aBlock [
    | spec projectSpec |
    projectSpec := self project projectSpec
        name: aString;
        yourself.
    spec := self project projectReferenceSpec
        name: aString;
        projectReference: projectSpec;
        yourself.
    self root packages merge: spec.
    self with: projectSpec during: aBlock
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setProject: aString withString: versionString [

	| spec projectSpec |
	projectSpec := 
		(self project projectSpec)
			name: aString;
			versionString: versionString;
			yourself.
	spec := 
		(self project projectReferenceSpec)
			name: aString;
			projectReference: projectSpec;
			yourself.
	self root packages merge: spec.
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setTimestampWithBlock: aBlock [

	| spec |
	(spec := self root getTimestamp) == nil
		ifTrue: [ 
			spec := self project valueHolderSpec.
			self root setTimestamp: spec ].
	self with: spec during: aBlock
]

{ #category : #'api callbacks' }
MetacelloAbstractVersionConstructor >> setTimestampWithString: aString [

	self root timestamp: aString
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> supplyingAnswers: aCollection [
    self root supplyingAnswers: aCollection constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> supplyingAnswersForPackage: anObject [
    self root answers: anObject
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> symbolicVersion [

	^symbolicVersion
]

{ #category : #accessing }
MetacelloAbstractVersionConstructor >> symbolicVersion: aSymbol [

	symbolicVersion := aSymbol
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> timestamp: aBlockOrStringOrDateAndTime [
    "Define timestamp field of version spec (MetacelloMCVersionSpec).
	 If <aBlockOrStringOrDateAndTime> is a String, the version spec timetamp is set to the String.
	 If <aBlockOrStringOrDateAndTime> is a DateAndTime, the version spec timetamp is set to the printString of the DateAndTime.
	If <aBlockOrStringOrDateAndTime> is a Block, the specifications in <aBlockOrStringOrDateAndTime> are applied to the timestamp spec (MetacelloValueHolderSpec). Not Recommended!
	
		spec timestamp: '10/7/2009 14:40'.
		
		spec timestamp: DateAndTime now'.
		
		spec timestamp: [
			spec value: '10/7/2009 14:40'. ].
    "

    self root timestamp: aBlockOrStringOrDateAndTime constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> timestampForVersion: aBlockOrStringOrDateAndTime [
    aBlockOrStringOrDateAndTime setTimestampInMetacelloConfig: self
]

{ #category : #validation }
MetacelloAbstractVersionConstructor >> validateDoItSelector: anObject [

	anObject == nil ifTrue: [ ^ self ].
	anObject isSymbol ifFalse: [ self error: 'Invalid message selector for doit: ', anObject printString ].
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> value: anObject [
    self root value: anObject constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> valueForValueHolder: anObject [
    self root value: anObject
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> version: anObject [
    self root version: anObject constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> versionForProject: anObject [
    self versionStringForProject: anObject
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> versionForVersion: anObject [
    self versionStringForVersion: anObject
]

{ #category : #api }
MetacelloAbstractVersionConstructor >> versionString: anObject [
    self root versionString: anObject constructor: self
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> versionStringForProject: anObject [
    self root versionString: anObject
]

{ #category : #'api spec callbacks' }
MetacelloAbstractVersionConstructor >> versionStringForVersion: anObject [
    self versionStringForProject: anObject
]

{ #category : #private }
MetacelloAbstractVersionConstructor >> with: aMetacelloSpec during: aBlock [
    | previousRoot |
    previousRoot := self root.
    self root: aMetacelloSpec.
    aBlock
        ensure: [ self root: previousRoot ]
]
