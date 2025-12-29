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

{ #category : 'api - old' }
MetacelloBaselineConstructor >> author: aBlockOrString [
	"Kept for compiatibility with other Smalltalk and old Pharo versions."

	MetacelloNotification signal:
		'The option #author: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: The author metadata was used with SmalltalkHub but not this information is already managed by git or other CVS.'
]

{ #category : 'api' }
MetacelloBaselineConstructor >> baseline: aString [
	"Alias to make baselines more explicits and backward compatible"

	self project: aString
]

{ #category : 'api' }
MetacelloBaselineConstructor >> baseline: aString with: aBlock [
	"Alias to make baselines more explicits and backward compatible"

	^ self project: aString with: aBlock
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> blessing: aBlockOrString [
	"Kept for compiatibility with other Smalltalk and old Pharo versions."

	MetacelloNotification signal:
		'The option #blessing: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: The author metadata was used with SmalltalkHub but not this information is already managed by git or other CVS.'
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

	configuration := aConfig
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> configuration: aString with: aBlock [

	MetacelloNotification signal:
		'The option #configuration:with: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: This option was here for configurations but not baselines. Since we cannot load configurations anymore in Pharo, it is been removed.'
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> configurationClass [

	^self configuration class
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> description: aBlockOrString [
	"Kept for compiatibility with other Smalltalk and old Pharo versions."

	MetacelloNotification signal:
		'The option #description: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: The author metadata was used with SmalltalkHub but not this information is already managed by git or other CVS.'
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

{ #category : 'api - old' }
MetacelloBaselineConstructor >> file: aString [
	"Kept for compiatibility with other Smalltalk and old Pharo versions."

	MetacelloNotification signal:
		'The option #file: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: This option was here for configurations but not baselines. Since we cannot load configurations anymore in Pharo, it is been removed.'
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

{ #category : 'api - old' }
MetacelloBaselineConstructor >> for: attributeListOrSymbol version: aString [

	MetacelloNotification signal:
		'The option #for:version: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: This option was here for configurations but not baselines. Since we cannot load configurations anymore in Pharo, it is been removed.'
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

{ #category : 'initialization' }
MetacelloBaselineConstructor >> on: aConfig project: aProject [

	| pragma versionSpec |
	project := aProject.
	self configuration: aConfig.
	pragma := self extractBaselinePragmaFor: aConfig class.
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
MetacelloBaselineConstructor >> package: packageName with: aBlock [
	"Define specification for package <packageName>.
	 If <aBlockOrString> is a String (or Symbol), the String is expected to be a version (or symbolic version).
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the project:
	
		spec package: 'MyPackage' with: '1.0'.
		
		spec package: 'MyPackage' with: [
			spec repository: '/opt/gemstone/repository'.
	 "

	| spec |
	spec := self project packageSpec
		        name: packageName;
		        yourself.

	self root packages merge: spec.
	aBlock isString
		ifTrue: [ MetacelloNotification signal: 'Now #package:with: only takes a block as second parameter and not a string.' ]
		ifFalse: [ self with: spec during: aBlock ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> postLoadDoIt: aSymbol [

	self root postLoadDoIt: aSymbol
]

{ #category : 'api' }
MetacelloBaselineConstructor >> preLoadDoIt: aSymbol [

	self root preLoadDoIt: aSymbol
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> project [

	^ project ifNil: [ project := MetacelloProject new ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> project: aString [

	self project: aString with: [ ]
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

	| projectSpec spec |
	projectSpec := self project projectSpec
		               name: aString;
		               yourself.

	spec := self project projectReferenceSpec
		        name: aString;
		        projectReference: projectSpec;
		        yourself.

	self root packages merge: spec.

	self with: projectSpec during: aBlockOrString
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

{ #category : 'api - old' }
MetacelloBaselineConstructor >> supplyingAnswers: aCollection [
	"Kept for compiatibility with other Smalltalk and old Pharo versions."

	MetacelloNotification signal:
		'The option #supplyingAnswer: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: The author metadata was used with SmalltalkHub but not this information is already managed by git or other CVS.'
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> timestamp: aBlockOrStringOrDateAndTime [
	"Kept for compiatibility with other Smalltalk and old Pharo versions."

	MetacelloNotification signal:
		'The option #timestamp: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: The author metadata was used with SmalltalkHub but not this information is already managed by git or other CVS.'
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> versionString: anObject [
	"Kept for compiatibility with other Smalltalk and old Pharo versions."

	MetacelloNotification signal:
		'The option #versionString:: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: This option was here for configurations but not baselines. Since we cannot load configurations anymore in Pharo, it is been removed.'
]

{ #category : 'private' }
MetacelloBaselineConstructor >> with: aMetacelloSpec during: aBlock [

	| previousRoot |
	previousRoot := self root.
	self root: aMetacelloSpec.
	aBlock ensure: [ self root: previousRoot ]
]
