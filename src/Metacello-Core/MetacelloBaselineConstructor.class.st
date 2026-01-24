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
MetacelloBaselineConstructor >> attributeMap [

	^ attributeMap
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> author: aBlockOrString [
	"Kept for compiatibility with other Smalltalk and old Pharo versions."

	MetacelloNotification signal:
		'The option #author: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: The author metadata was used with SmalltalkHub but not this information is already managed by git or other CVS.'
]

{ #category : 'baselines' }
MetacelloBaselineConstructor >> baseline: aString [

	self baseline: aString with: [ ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> baseline: aString with: aBlock [
	"Alias to make baselines more explicits and backward compatible.
	
	I allow to decalre a dependency to a project. The string will be the project name. In the block some things can be set such as the repository, the things to load (using #loads:) and other parameters.
	By default if the #className is not defined, it will use BaselineOf{nameOfTheProject}."

	^ self project: aString with: aBlock
]

{ #category : 'pragma extraction' }
MetacelloBaselineConstructor >> baselineMethodIn: aClass [

	^ (Pragma allNamed: #baseline from: aClass to: BaselineOf)
		ifEmpty: [ self error: 'No #baseline pragma found' ]
		ifNotEmpty: [ :pragmas | pragmas first methodSelector ]
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> blessing: aBlockOrString [
	"Kept for compiatibility with other Smalltalk and old Pharo versions."

	MetacelloNotification signal:
		'The option #blessing: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: The author metadata was used with SmalltalkHub but not this information is already managed by git or other CVS.'
]

{ #category : 'api' }
MetacelloBaselineConstructor >> className: aString [
	"Define className field of a project spec (MetacelloProjectSpec).
	
		spec project: 'CoolBrowser' with: [
			spec className: 'BaselineOfCoolBrowser'. ].

	The className field is OPTIONAL in the project spec. If omitted, the className will be created by prepending 'BaselineOf' to the project name.
	 "

	self root className: aString
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> configuration: aString with: aBlock [

	MetacelloNotification signal:
		'The option #configuration:with: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: This option was here for configurations but not baselines. Since we cannot load configurations anymore in Pharo, it is been removed.'
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> description: aBlockOrString [
	"Kept for compiatibility with other Smalltalk and old Pharo versions."

	MetacelloNotification signal:
		'The option #description: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: The author metadata was used with SmalltalkHub but not this information is already managed by git or other CVS.'
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> file: aString [
	"Kept for compiatibility with other Smalltalk and old Pharo versions."

	MetacelloNotification signal:
		'The option #file: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: This option was here for configurations but not baselines. Since we cannot load configurations anymore in Pharo, it is been removed.'
]

{ #category : 'api' }
MetacelloBaselineConstructor >> for: attributeListOrSymbol do: aBlock [
	"conditional version support.
	
	The block will be executed only if the attribute list has one attribute matches an attribute of the current load."

	(attributeListOrSymbol isSymbol
		 ifTrue: [ { attributeListOrSymbol } ]
		 ifFalse: [ attributeListOrSymbol ]) do: [ :attribute | (self attributeMap at: attribute ifAbsentPut: [ OrderedCollection new ]) add: aBlock ]
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> for: attributeListOrSymbol version: aString [

	MetacelloNotification signal:
		'The option #for:version: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: This option was here for configurations but not baselines. Since we cannot load configurations anymore in Pharo, it is been removed.'
]

{ #category : 'api' }
MetacelloBaselineConstructor >> group: aString overrides: aStringOrCollection [
	"Define a groupe to replace an existing group"

	self root packagesSpec add: (self project groupSpec
			 name: aString;
			 includes: aStringOrCollection;
			 yourself)
]

{ #category : 'api' }
MetacelloBaselineConstructor >> group: aString with: aStringOrCollection [
	"Adds the dependencies as parameter to the group named `aString`. If the group does not yet exist, it will be created."

	self root packagesSpec merge: (self project groupSpec
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
		
		This can be used when we declare projects at the root and import them in some subsections (using `for:do:`).
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

{ #category : 'initialization' }
MetacelloBaselineConstructor >> initialize [

	super initialize.
	attributeMap := Dictionary new
]

{ #category : 'api' }
MetacelloBaselineConstructor >> loads: aStringOrCollection [
	"Define the list of loadable specs to load. 
	
	This is sued when declaring a dependency to another project. It is possible to not load the full project but to load only a subset."

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
MetacelloBaselineConstructor >> on: aBaseline project: aProject [

	| versionSpec |
	project := aProject.
	self project baseline: aBaseline.
	versionSpec := MetacelloVersionSpec project: self project.
	self root: versionSpec.
	aBaseline perform: (self baselineMethodIn: aBaseline class) with: self.
	self project attributes do: [ :attribute |
		(self attributeMap at: attribute ifAbsent: [ ]) ifNotNil: [ :blockList | blockList do: [ :block | self with: versionSpec during: block ] ] ].
	self project version: versionSpec
]

{ #category : 'api - old' }
MetacelloBaselineConstructor >> operator: anObject [

	MetacelloNotification signal:
		'The option #operator: will not do anything anymore in Pharo 14+. This option can be removed in it is in an attribute for Pharo 14+ or ignored if this is for a different platform. Reason: This option was here for configurations but not baselines. Since we cannot load configurations anymore in Pharo, it is been removed.'
]

{ #category : 'api' }
MetacelloBaselineConstructor >> package: aString [
	"Decalre a package to load"

	self root packagesSpec add: (self project packageSpec
			 name: aString;
			 yourself)
]

{ #category : 'api' }
MetacelloBaselineConstructor >> package: aString overrides: aBlock [
	"Replace the declaration of a package and its dependencies by this new declaration"

	| spec |
	spec := self project packageSpec
		        name: aString;
		        yourself.

	self with: spec during: aBlock.

	self root packagesSpec add: spec
]

{ #category : 'api' }
MetacelloBaselineConstructor >> package: packageName with: aBlock [
	"Define specification for package <packageName>.
	 If <aBlockOrString> is a String (or Symbol), the String is expected to be a version (or symbolic version).
	If <aBlockOrString> is a Block, the specifications in <aBlockOrString> are applied to the project:
	
		spec package: 'MyPackage' with: '1.0'.
		
		spec package: 'MyPackage' with: [
			spec repository: '/opt/gemstone/repository'.
			
	If the package declaration already exists, merge the two declarations.
	 "

	| spec |
	spec := self project packageSpec
		        name: packageName;
		        yourself.

	aBlock isString
		ifTrue: [ MetacelloNotification signal: 'Now #package:with: only takes a block as second parameter and not a string.' ]
		ifFalse: [ self with: spec during: aBlock ].

	self root packagesSpec merge: spec
]

{ #category : 'api' }
MetacelloBaselineConstructor >> postLoadDoIt: aSymbol [
	"The symbol should match a method name in the baseline. This method will be executed after the loading of the spec."

	self root postLoadDoIt: aSymbol
]

{ #category : 'api' }
MetacelloBaselineConstructor >> preLoadDoIt: aSymbol [
	"The symbol should match a method name in the baseline. This method will be executed before the loading of the spec (but after the fetching phase)."

	self root preLoadDoIt: aSymbol
]

{ #category : 'accessing' }
MetacelloBaselineConstructor >> project [

	^ project ifNil: [ project := MetacelloProject new ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> project: aString [
	"Allow to declare a dependency to a project. This version without a #with: argument is seldom used"

	self project: aString with: [ ]
]

{ #category : 'api' }
MetacelloBaselineConstructor >> project: aString copyFrom: oldSpecName with: aBlock [
	"I allow to copy a dependency declaration and to adapt it.
	Typically I'll be used to load different groups of a project. 
	
	Example:
	
	spec project: 'Toto' with: [
		spec
			repository: 'uri';
			loads: #(Core)
		].
	
	spec project: 'TotoTests' copyFrom: 'Toto' with: [ spec loads: #(Tests) ].
	
	spec package: 'Model' with: [ spec requires: 'Toto' ].
	spec package: 'Model-Tests' with: [ spec requires: #(Model TotoTests) ]"

	| spec projectSpec |
	projectSpec := MetacelloProjectSpec named: aString.
	spec := MetacelloProjectReferenceSpec projectReference: projectSpec.
	self with: projectSpec during: aBlock.

	self root packagesSpec copy: oldSpecName to: spec
]

{ #category : 'api' }
MetacelloBaselineConstructor >> project: aString overrides: aBlock [
	"Override a project declaration"

	| spec projectSpec |
	projectSpec := MetacelloProjectSpec named: aString.
	spec := MetacelloProjectReferenceSpec projectReference: projectSpec.
	self with: projectSpec during: aBlock.
	self root packagesSpec add: spec
]

{ #category : 'api' }
MetacelloBaselineConstructor >> project: aString with: aBlockOrString [
	"I am used to declare a dependency on a project.
	I allow to decalre a dependency to a project. The string will be the project name. In the block some things can be set such as the repository, the things to load (using #loads:) and other parameters.
	By default if the #className is not defined, it will use BaselineOf{nameOfTheProject}."

	| projectSpec spec |
	projectSpec := MetacelloProjectSpec named: aString.
	spec := MetacelloProjectReferenceSpec projectReference: projectSpec.

	self with: projectSpec during: aBlockOrString.

	self root packagesSpec merge: spec
]

{ #category : 'api' }
MetacelloBaselineConstructor >> projectPackage: aBlock [
	"projectPackage spec data folded into project spec"

	self with: self root during: aBlock
]

{ #category : 'api' }
MetacelloBaselineConstructor >> removeGroup: aString [

	self root packagesSpec remove: (self project groupSpec
			 name: aString;
			 yourself)
]

{ #category : 'api' }
MetacelloBaselineConstructor >> removePackage: aString [

	self root packagesSpec remove: (self project packageSpec
			 name: aString;
			 yourself)
]

{ #category : 'api' }
MetacelloBaselineConstructor >> removeProject: aString [

	self root packagesSpec remove: (MetacelloProjectReferenceSpec projectReference: (self root packageNamed: aString))
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

{ #category : 'accessing' }
MetacelloBaselineConstructor >> strict: aBoolean [
	
	root strict: aBoolean
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
