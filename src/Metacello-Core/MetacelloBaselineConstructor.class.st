"
##MetacelloBaselineConstructor

**MetacelloBaselineConstructor** extracts the #baseline pragma from a **BaselineOfConfiguration** and produces a **MetacelloVersion**:.

```Smalltalk
MetacelloBaselineConstructor on: BaselineOfExample
```

"
Class {
	#name : 'MetacelloBaselineConstructor',
	#superclass : 'Object',
	#instVars : [
		'root',
		'attributeOrder',
		'configuration',
		'project',
		'attributeMap'
	],
	#category : 'Metacello-Core-Constructors',
	#package : 'Metacello-Core',
	#tag : 'Constructors'
}

{ #category : 'class initialization' }
MetacelloBaselineConstructor class >> initialize [

	"Aliases added during the development of Pharo14. We can remove this in a few version of Pharo"
	self deprecatedAliases: { #MetacelloAbstractVersionConstructor. #MetacelloVersionConstructor }
]

{ #category : 'instance creation' }
MetacelloBaselineConstructor class >> on: aConfig [

	^ self on: aConfig project: nil
]

{ #category : 'instance creation' }
MetacelloBaselineConstructor class >> on: aConfig project: aProject [

	^ self new
		  on: aConfig project: aProject;
		  yourself
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> addAttribute: anAttribute [

	self attributeOrder add: anAttribute
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> attributeMap [

	^ attributeMap ifNil: [ attributeMap := Dictionary new ]
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> attributeOrder [

	^ attributeOrder ifNil: [ attributeOrder := OrderedCollection new ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> baseline: aString [

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

{ #category : 'api' }
MetacelloBaselineConstructor >> baseline: aString with: aBlock [

	self with: (self baseline: aString) during: aBlock
]

{ #category : 'api' }
MetacelloBaselineConstructor >> className: aString [
	"Define className field of a project spec (MetacelloMCProjectSpec).
	
		spec project: 'CoolBrowser' with: [
			spec className: 'ConfigurationOfCoolBrowser'. ].

	The className field is OPTIONAL in the project spec. If omitted, the className will be created by prepending 'ConfigurationOf' to the project name.
	 "

	self root className: aString
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> configuration [

	^configuration
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> configuration: aConfig [

	self flag: #ConfigurationRemoval. "This method should probably be renamed since it does not contain a configuration anymore."
	configuration := aConfig
]

{ #category : 'private' }
MetacelloBaselineConstructor >> evaluatePragma: pragma [

	self configuration perform: pragma methodSelector with: self
]

{ #category : 'pragma extraction' }
MetacelloBaselineConstructor >> extractBaselinePragmaFor: aClass [

	^ (Pragma allNamed: #baseline in: aClass)
		ifEmpty: [ self error: 'No #baseline pragma found' ]
		ifNotEmpty: [ :pragmas | pragmas first ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> file: aString [
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
MetacelloBaselineConstructor >> for: attributeListOrSymbol do: aBlock [
	"conditional version support"

	(attributeListOrSymbol isSymbol
		 ifTrue: [ { attributeListOrSymbol } ]
		 ifFalse: [ attributeListOrSymbol ]) do: [ :attribute |
		(self attributeMap at: attribute ifAbsentPut: [ OrderedCollection new ]) add: aBlock.
		self addAttribute: attribute ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> for: attributeListOrSymbol version: aString [
	"conditional symbolicVersion support"

	(attributeListOrSymbol isSymbol
		 ifTrue: [ { attributeListOrSymbol } ]
		 ifFalse: [ attributeListOrSymbol ]) do: [ :attribute |
		self attributeMap at: attribute put: aString.
		self addAttribute: attribute ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> group: aString overrides: aStringOrCollection [

	self root packages add: (self project groupSpec
			 name: aString;
			 includes: aStringOrCollection;
			 yourself)
]

{ #category : 'api' }
MetacelloBaselineConstructor >> group: aString with: aStringOrCollection [

	self root packages merge: (self project groupSpec
			 name: aString;
			 includes: aStringOrCollection;
			 yourself)
]

{ #category : 'api' }
MetacelloBaselineConstructor >> import: aStringOrCollection [
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
MetacelloBaselineConstructor >> import: aString provides: aCollection [
	"import names defined in baseline <aString> to be used when loading the version

	
		spec baseline: 'Sample' with: [ spec repository: 'github://dalehenrich/sample:master/repository' ].
		spec import: 'Sample' provides: #('Sample Core');
		spec baseline: 'Example' with: [ spec repository: 'github://dalehenrich/example:master/repository' ].
		spec import: 'Example' provides: #('Example Core')]	
	 "

	self root import: aString provides: aCollection
]

{ #category : 'api' }
MetacelloBaselineConstructor >> includes: anObject [

	self root includes: anObject
]

{ #category : 'api' }
MetacelloBaselineConstructor >> loads: aStringOrCollection [

	self root loads: aStringOrCollection
]

{ #category : 'api' }
MetacelloBaselineConstructor >> loads: aCollection from: aString [

	self
		loads: aCollection;
		repository: aString
]

{ #category : 'api' }
MetacelloBaselineConstructor >> name: anObject [

	self root name: anObject
]

{ #category : 'private' }
MetacelloBaselineConstructor >> on: aConfig project: aProject [

	| pragma versionSpec |
	self configuration: aConfig.
	pragma := self extractBaselinePragmaFor: aConfig class.
	project := aProject ifNil: [ MetacelloBaselineProject new ].
	versionSpec := self project versionSpec.
	self root: versionSpec.
	self evaluatePragma: pragma.
	self project attributes do: [ :attribute |
		(self attributeMap at: attribute ifAbsent: [ ]) ifNotNil: [ :blockList | blockList do: [ :block | self with: versionSpec during: block ] ] ].
	self project version: versionSpec createVersion.
	self project configuration: aConfig
]

{ #category : 'api' }
MetacelloBaselineConstructor >> operator: anObject [

	self root operator: anObject
]

{ #category : 'api' }
MetacelloBaselineConstructor >> package: aString [

	self root packages add: (self project packageSpec
			 name: aString;
			 yourself)
]

{ #category : 'api' }
MetacelloBaselineConstructor >> package: aString overrides: aBlock [

	| spec |
	spec := self project packageSpec
		        name: aString;
		        yourself.
	self root packages add: spec.
	self with: spec during: aBlock
]

{ #category : 'api' }
MetacelloBaselineConstructor >> package: packageName with: aBlockOrString [
	"Define specification for package <packageName>.
	 If <aBlockOrString> is a String (or Symbol), the String is expected to be a version (or symbolic version).
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the project:
	
		spec package: 'MyPackage' with: '1.0'.
		
		spec package: 'MyPackage' with: [
			spec file:'MyPackage-dkh.1'.
			spec repository: '/opt/gemstone/repository'.
	 "

	| spec |
	spec := self project packageSpec
		        name: packageName;
		        yourself.

	aBlockOrString isString
		ifTrue: [
			spec file: aBlockOrString.
			self root packages merge: spec ]
		ifFalse: [
			self root packages merge: spec.
			self with: spec during: aBlockOrString ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> postLoadDoIt: aSymbol [

	self validateDoItSelector: aSymbol.
	self root postLoadDoIt: aSymbol
]

{ #category : 'api' }
MetacelloBaselineConstructor >> preLoadDoIt: aSymbol [

	self validateDoItSelector: aSymbol.
	self root preLoadDoIt: aSymbol
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> project [

	^ project
]

{ #category : 'api' }
MetacelloBaselineConstructor >> project: aString [

	self project: aString with: ''
]

{ #category : 'api' }
MetacelloBaselineConstructor >> project: aString copyFrom: oldSpecName with: aBlock [

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
MetacelloBaselineConstructor >> project: aString overrides: aBlock [

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
MetacelloBaselineConstructor >> project: aString with: aBlockOrString [

	| projectSpec |
	projectSpec := self project projectSpec
		               name: aString;
		               yourself.
	self root packages merge: (self project projectReferenceSpec
			 name: aString;
			 projectReference: projectSpec;
			 yourself).
	aBlockOrString isString
		ifTrue: [ projectSpec versionString: aBlockOrString ]
		ifFalse: [ self with: projectSpec during: aBlockOrString ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> projectPackage: aBlock [
	"projectPackage spec data folded into project spec"

	self with: self root during: aBlock
]

{ #category : 'api' }
MetacelloBaselineConstructor >> removeGroup: aString [

	self root packages remove: (self project groupSpec
			 name: aString;
			 yourself)
]

{ #category : 'api' }
MetacelloBaselineConstructor >> removePackage: aString [

	self root packages remove: (self project packageSpec
			 name: aString;
			 yourself)
]

{ #category : 'api' }
MetacelloBaselineConstructor >> removeProject: aString [

	self root packages remove: (self project projectReferenceSpec
			 name: aString;
			 yourself)
]

{ #category : 'api' }
MetacelloBaselineConstructor >> repositories: aBlock [

	self with: self root repositories during: aBlock
]

{ #category : 'api' }
MetacelloBaselineConstructor >> repository: anObject [

	self root repository: anObject
]

{ #category : 'api' }
MetacelloBaselineConstructor >> repository: description username: username password: password [

	self root repository: description username: username password: password
]

{ #category : 'loading' }
MetacelloBaselineConstructor >> repositoryDescriptions [
	
	^ self project repositoryDescription
]

{ #category : 'api' }
MetacelloBaselineConstructor >> requires: anObject [

	self root requires: anObject
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> root [

	^root
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> root: aMetacelloSpec [
    root := aMetacelloSpec
]

{ #category : 'api' }
MetacelloBaselineConstructor >> supplyingAnswers: aCollection [

	self flag: #ConfigurationRemoval. "I think this method should not be on baselines but let's see that later. After cleaning the code it will be easier to see if this is still used."
	self root answers: aCollection
]

{ #category : 'validation' }
MetacelloBaselineConstructor >> validateDoItSelector: anObject [

	anObject ifNil: [ ^ self ].
	anObject isSymbol ifFalse: [ self error: 'Invalid message selector for doit: ' , anObject printString ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> value: anObject [

	self root value: anObject
]

{ #category : 'api' }
MetacelloBaselineConstructor >> versionString: anObject [

	self root versionString: anObject
]

{ #category : 'private' }
MetacelloBaselineConstructor >> with: aMetacelloSpec during: aBlock [

	| previousRoot |
	previousRoot := self root.
	self root: aMetacelloSpec.
	aBlock ensure: [ self root: previousRoot ]
]
