Class {
	#name : 'MetacelloAbstractVersionConstructor',
	#superclass : 'Object',
	#instVars : [
		'root',
		'configuration',
		'project',
		'attributeMap',
		'attributeOrder',
		'symbolicVersion',
		'currentContext'
	],
	#category : 'Metacello-Core-Constructors',
	#package : 'Metacello-Core',
	#tag : 'Constructors'
}

{ #category : 'instance creation' }
MetacelloAbstractVersionConstructor class >> on: aConfig [

	^ self new
		  on: aConfig;
		  yourself
]

{ #category : 'instance creation' }
MetacelloAbstractVersionConstructor class >> on: aConfig project: aProject [

	^ self new
		  on: aConfig project: aProject;
		  yourself
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> addAttribute: anAttribute [

	self attributeOrder add: anAttribute
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> attributeMap [

	^ attributeMap ifNil: [ attributeMap := Dictionary new ]
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> attributeOrder [

	^ attributeOrder ifNil: [ attributeOrder := OrderedCollection new ]
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> author: aBlockOrString [
	"Define author field of version spec (MetacelloMCVersionSpec).
	 If <aBlockOrString> is a String, the version spec author is set to the String.
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the author spec (MetacelloValueHolderSpec). Not Recommended!
	
		spec author: 'dkh'.
		
		spec author: [
			spec value: 'dkh'. ].
	 "

	aBlockOrString setAuthorInMetacelloConfig: self
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> baseline: aString [

	self setBaseline: aString
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> baseline: aString with: aBlock [

	aBlock setBaseline: aString withInMetacelloConfig: self
]

{ #category : 'api' }
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

	aBlockOrString setBlessingInMetacelloConfig: self
]

{ #category : 'initialization' }
MetacelloAbstractVersionConstructor >> calculate: aConfig project: aProject [ 
	^ self subclassResponsibility
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> className: aString [
	"Define className field of a project spec (MetacelloMCProjectSpec).
	
		spec project: 'CoolBrowser' with: [
			spec className: 'ConfigurationOfCoolBrowser'. ].

	The className field is OPTIONAL in the project spec. If omitted, the className will be created by prepending 'ConfigurationOf' to the project name.
	 "

	self root className: aString
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> configuration [

	^configuration
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> configuration: aConfig [

	configuration := aConfig
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> configuration: aString with: aBlockOrString [

	aBlockOrString setConfiguration: aString withInMetacelloConfig: self
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> configurationClass [

	^self configuration class
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> description: aBlockOrString [
	"Define description field of version spec (MetacelloMCVersionSpec).
	 If <aBlockOrString> is a String, the version spec blessing is set to the String. It is recommended to use a Symbol.
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the blessing spec (MetacelloValueHolderSpec). Not Recommended!
	
		spec description: 'Descriptive comment'.
		
		spec description: [
			spec value: 'Descriptive comment'.
	 "

	aBlockOrString setDescriptionInMetacelloConfig: self
]

{ #category : 'private' }
MetacelloAbstractVersionConstructor >> evaluatePragma: pragma [

	currentContext := pragma.
	[ self configuration perform: pragma methodSelector with: self ] ensure: [ currentContext := nil ]
]

{ #category : 'pragma extraction' }
MetacelloAbstractVersionConstructor >> extractAllVersionPragmas [

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #version: into: aDict.
	self extractPragmas: #version:imports: into: aDict.
	^aDict
]

{ #category : 'pragma extraction' }
MetacelloAbstractVersionConstructor >> extractCommonDefaultSymbolicVersionPragmas [

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #defaultSymbolicVersion: for: ConfigurationOf into: aDict.
	^aDict
]

{ #category : 'pragma extraction' }
MetacelloAbstractVersionConstructor >> extractDefaultSymbolicVersionPragmas [

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #defaultSymbolicVersion: into: aDict.
	^aDict
]

{ #category : 'private' }
MetacelloAbstractVersionConstructor >> extractPragmas: pragmaKeyword for: aClass into: versionDict [

	| versionString  pragmas |
	(Pragma 
		allNamed: pragmaKeyword
		in: aClass) do: [:pragma |
			versionString := pragma argumentAt: 1.
			pragmas := versionDict 
				at: versionString 
				ifAbsent: [ | list |
					list := OrderedCollection new.
					versionDict at: versionString put: list.
					list ].
			pragmas add: pragma ].
]

{ #category : 'private' }
MetacelloAbstractVersionConstructor >> extractPragmas: pragmaKeyword into: versionDict [

	^self extractPragmas: pragmaKeyword for: self configurationClass into: versionDict
]

{ #category : 'pragma extraction' }
MetacelloAbstractVersionConstructor >> extractSymbolicVersionPragmas [

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #symbolicVersion: into: aDict.
	^aDict
]

{ #category : 'pragma extraction' }
MetacelloAbstractVersionConstructor >> extractVersionImportPragmas [

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #version:imports: into: aDict.
	^aDict
]

{ #category : 'pragma extraction' }
MetacelloAbstractVersionConstructor >> extractVersionPragmas [

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #version: into: aDict.
	^aDict
]

{ #category : 'api' }
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

	self root file: aString
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> for: attributeListOrSymbol do: aBlock [
	"conditional version support"

	(attributeListOrSymbol isSymbol
		 ifTrue: [ { attributeListOrSymbol } ]
		 ifFalse: [ attributeListOrSymbol ]) do: [ :attribute |
		(self attributeMap at: attribute ifAbsentPut: [ OrderedCollection new ]) add: aBlock.
		self addAttribute: attribute ]
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> for: attributeListOrSymbol version: aString [
	"conditional symbolicVersion support"

	(attributeListOrSymbol isSymbol
		 ifTrue: [ { attributeListOrSymbol } ]
		 ifFalse: [ attributeListOrSymbol ]) do: [ :attribute |
		self attributeMap at: attribute put: aString.
		self addAttribute: attribute ]
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> group: aString overrides: aStringOrCollection [

	self root packages add: (self project groupSpec
			 name: aString;
			 includes: aStringOrCollection;
			 yourself)
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> group: aString with: aStringOrCollection [

	self root packages merge: (self project groupSpec
			 name: aString;
			 includes: aStringOrCollection;
			 yourself)
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> import: aStringOrCollection [
	"import names defined in baseline <aString> to be used when loading the version

		spec baseline: 'Sample' with: [ spec repository: 'github://dalehenrich/sample:master/repository' ].
		spec import: 'Sample' ]
	
	or a list of names for multiple project imports:
	
		spec baseline: 'Sample' with: [ spec repository: 'github://dalehenrich/sample:master/repository' ].
		spec baseline: 'Example' with: [ spec repository: 'github://dalehenrich/example:master/repository' ].
		spec import: #('Sample' 'Example')]	
	 "

	self root import: aStringOrCollection
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> import: aString provides: aCollection [
	"import names defined in baseline <aString> to be used when loading the version

	
		spec baseline: 'Sample' with: [ spec repository: 'github://dalehenrich/sample:master/repository' ].
		spec import: 'Sample' provides: #('Sample Core');
		spec baseline: 'Example' with: [ spec repository: 'github://dalehenrich/example:master/repository' ].
		spec import: 'Example' provides: #('Example Core')]	
	 "

	self root import: aString provides: aCollection
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> includes: anObject [

	self root includes: anObject
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> loads: aStringOrCollection [

	self root loads: aStringOrCollection
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> name: anObject [

	self root name: anObject
]

{ #category : 'initialization' }
MetacelloAbstractVersionConstructor >> on: aConfig [

	self calculate: aConfig project: nil
]

{ #category : 'initialization' }
MetacelloAbstractVersionConstructor >> on: aConfig project: aProject [

	self calculate: aConfig project: aProject
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> operator: anObject [

	self root operator: anObject
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> package: aString [

	self root packages add: (self project packageSpec
			 name: aString;
			 yourself)
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> package: aString overrides: aBlock [

	| spec |
	spec := self project packageSpec
		        name: aString;
		        yourself.
	self root packages add: spec.
	self with: spec during: aBlock
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> package: packageName with: aBlockOrString [
	"Define specification for package <packageName>.
	 If <aBlockOrString> is a String (or Symbol), the String is expected to be a version (or symbolic version).
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the project:
	
		spec package: 'MyPackage' with: '1.0'.
		
		spec package: 'MyPackage' with: [
			spec file:'MyPackage-dkh.1'.
			spec repository: '/opt/gemstone/repository'.
	 "

	aBlockOrString setPackage: packageName withInMetacelloConfig: self
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> postLoadDoIt: aSymbol [

	self validateDoItSelector: aSymbol.
	self root postLoadDoIt: aSymbol
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> preLoadDoIt: aSymbol [

	self validateDoItSelector: aSymbol.
	self root preLoadDoIt: aSymbol
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> project [

	^ project ifNil: [ project := self projectClass new ]
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> project: aString [

	self project: aString with: ''
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> project: aString copyFrom: oldSpecName with: aBlock [

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

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> project: aString overrides: aBlock [

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

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> project: aString with: aBlockOrString [

	aBlockOrString setProject: aString withInMetacelloConfig: self
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> projectClass [

	^ MetacelloProject
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> projectPackage: aBlock [
	"projectPackage spec data folded into project spec"

	self with: self root during: aBlock
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> removeGroup: aString [

	self root packages remove: (self project groupSpec
			 name: aString;
			 yourself)
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> removePackage: aString [

	self root packages remove: (self project packageSpec
			 name: aString;
			 yourself)
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> removeProject: aString [

	self root packages remove: (self project projectReferenceSpec
			 name: aString;
			 yourself)
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> repositories: aBlock [

	self with: self root repositories during: aBlock
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> repository: anObject [

	self root repository: anObject
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> repository: description username: username password: password [

	self root repository: description username: username password: password
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> requires: anObject [

	self root requires: anObject
]

{ #category : 'initialization' }
MetacelloAbstractVersionConstructor >> reset [
    attributeMap := attributeOrder := nil
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> root [

	^root
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> root: aMetacelloSpec [
    root := aMetacelloSpec
]

{ #category : 'api callbacks' }
MetacelloAbstractVersionConstructor >> setAuthorWithBlock: aBlock [

	| spec |
	(spec := self root getAuthor) ifNil: [
		spec := self project valueHolderSpec.
		self root setAuthor: spec ].
	self with: spec during: aBlock
]

{ #category : 'api callbacks' }
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

{ #category : 'api callbacks' }
MetacelloAbstractVersionConstructor >> setBaseline: aString withBlock: aBlock [
    | projectSpec |
    projectSpec := self setBaseline: aString.
    self with: projectSpec during: aBlock
]

{ #category : 'api callbacks' }
MetacelloAbstractVersionConstructor >> setBlessingWithBlock: aBlock [

	| spec |
	(spec := self root getBlessing) ifNil: [
		spec := self project valueHolderSpec.
		self root setBlessing: spec ].
	self with: spec during: aBlock
]

{ #category : 'api callbacks' }
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

{ #category : 'api callbacks' }
MetacelloAbstractVersionConstructor >> setDescriptionWithBlock: aBlock [

	| spec |
	(spec := self root getDescription) ifNil: [
		spec := self project valueHolderSpec.
		self root setDescription: spec ].
	self with: spec during: aBlock
]

{ #category : 'api callbacks' }
MetacelloAbstractVersionConstructor >> setPackage: aString withBlock: aBlock [

	| spec |
	spec := 
		(self project packageSpec)
			name: aString;
			yourself.
	self root packages merge: spec.
	self with: spec during: aBlock
]

{ #category : 'api callbacks' }
MetacelloAbstractVersionConstructor >> setPackage: aString withString: aFile [

	self root packages merge: (self project packageSpec
			 name: aString;
			 file: aFile;
			 yourself)
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> setProject: aProject [

	project := aProject
]

{ #category : 'api callbacks' }
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

{ #category : 'api callbacks' }
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

{ #category : 'api callbacks' }
MetacelloAbstractVersionConstructor >> setTimestampWithBlock: aBlock [

	| spec |
	(spec := self root getTimestamp) ifNil: [
		spec := self project valueHolderSpec.
		self root setTimestamp: spec ].
	self with: spec during: aBlock
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> supplyingAnswers: aCollection [

	self root answers: aCollection
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> symbolicVersion [

	^symbolicVersion
]

{ #category : 'accessing' }
MetacelloAbstractVersionConstructor >> symbolicVersion: aSymbol [

	symbolicVersion := aSymbol
]

{ #category : 'api' }
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

	aBlockOrStringOrDateAndTime setTimestampInMetacelloConfig: self
]

{ #category : 'validation' }
MetacelloAbstractVersionConstructor >> validateDoItSelector: anObject [

	anObject ifNil: [ ^ self ].
	anObject isSymbol ifFalse: [ self error: 'Invalid message selector for doit: ' , anObject printString ]
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> value: anObject [

	self root value: anObject
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> version: anObject [

	self root versionString: anObject
]

{ #category : 'api' }
MetacelloAbstractVersionConstructor >> versionString: anObject [

	self root versionString: anObject
]

{ #category : 'private' }
MetacelloAbstractVersionConstructor >> with: aMetacelloSpec during: aBlock [

	| previousRoot |
	previousRoot := self root.
	self root: aMetacelloSpec.
	aBlock ensure: [ self root: previousRoot ]
]
