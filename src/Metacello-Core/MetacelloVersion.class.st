Class {
	#name : 'MetacelloVersion',
	#superclass : 'Magnitude',
	#instVars : [
		'loaderPolicy',
		'versionStatus',
		'importedVersions',
		'spec',
		'versionNumber'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'instance creation' }
MetacelloVersion class >> fromSpec: aMetacelloVersionSpec [
    ^ (self new versionNumber: aMetacelloVersionSpec versionNumber)
        spec: aMetacelloVersionSpec;
        yourself
]

{ #category : 'instance creation' }
MetacelloVersion class >> fromString: aString [

	^self new versionNumber: aString asMetacelloVersionNumber
]

{ #category : 'class initialization' }
MetacelloVersion class >> initialize [ 
	self deprecatedAliases: { #MetacelloMCVersion }
]

{ #category : 'comparing' }
MetacelloVersion >> < aMetacelloVersion [

	aMetacelloVersion species = self species ifFalse: [ ^false ].
	^self versionNumber < aMetacelloVersion versionNumber
]

{ #category : 'comparing' }
MetacelloVersion >> = aMetacelloVersion [

	aMetacelloVersion species = self species ifFalse: [ ^false ].
	^self versionNumber = aMetacelloVersion versionNumber
]

{ #category : 'querying' }
MetacelloVersion >> author [

	^self spec author value
]

{ #category : 'accessing' }
MetacelloVersion >> basicSpec [
    ^ self spec
]

{ #category : 'querying' }
MetacelloVersion >> blessing [
    ^ self basicSpec blessing value
]

{ #category : 'private' }
MetacelloVersion >> computeVersionStatus [
	"
		#allLoadedToSpec - all projects and packages are loaded and match specification
		#loadedToSpec - all loaded projects and packages match specifications (at least one package loaded)
		#loadedMatchConstraints - all loaded projects and packages match constraints (at least one package loaded)
		#somethingLoaded - at least one package loaded
	"

	self spec computeVersionStatus: [ :status | ^ status ].
	^ #noStatus
]

{ #category : 'querying' }
MetacelloVersion >> defaultPackageNamesToLoad: defaultList [
	"Answer the list of packages and projects to be loaded: packages already loaded plust defaultList"
	
	^ self packageAndProjectNamesToLoad: defaultList loader: self loader
]

{ #category : 'querying' }
MetacelloVersion >> description [
    ^ self basicSpec description value
]

{ #category : 'querying' }
MetacelloVersion >> difference: aMetacelloVersion [
	"Return a a dictionary of additions, removals and modifications"
	
	^self spec difference: aMetacelloVersion spec
]

{ #category : 'private' }
MetacelloVersion >> doFetchRequiredFromArray: anArray [

	| oldPolicy oldBypassProgress displayString |
	displayString := self versionNumber printString, ' of ', self spec projectLabel.
	MetacelloNotification signal: ('Fetching ', displayString, '...').
	oldPolicy := loaderPolicy. 
	oldBypassProgress := MetacelloPlatform current bypassProgressBars.
	self loaderPolicy silently ifTrue: [ MetacelloPlatform current bypassProgressBars: true ].
	[ 	| ans |
		ans := self fetchRequiredFromArray: anArray.
		MetacelloNotification signal: ('...finished ', self versionNumber printString).
		^ans ]
			ensure: [ 
				MetacelloPlatform current bypassProgressBars: oldBypassProgress.
				loaderPolicy := oldPolicy ]
]

{ #category : 'private' }
MetacelloVersion >> doLoadRequiredFromArray: anArray [

	| displayString oldPolicy oldBypassProgress |
	displayString := self versionNumber printString, ' of ', self spec projectLabel.
	MetacelloNotification signal: ('Loading ', displayString, '...').
	oldPolicy := loaderPolicy. 
	oldBypassProgress := MetacelloPlatform current bypassProgressBars.
	self loaderPolicy silently ifTrue: [ MetacelloPlatform current bypassProgressBars: true ].
	[	| fetchLoader |
		fetchLoader := self fetchRequiredFromArray: (self defaultPackageNamesToLoad: anArray).
		MetacelloPlatform current
			do:  [  fetchLoader doLoad ]
			displaying: 'Loading ', displayString.
		MetacelloNotification signal: ('...finished ', self versionNumber printString).
		^fetchLoader ]
		ensure: [ 
			MetacelloPlatform current bypassProgressBars: oldBypassProgress.
			loaderPolicy := oldPolicy ]
]

{ #category : 'private' }
MetacelloVersion >> doRecordRequiredFromArray: anArray [

	| originalLoader oldPolicy displayString oldBypassProgress |
	displayString := self versionNumber printString, ' of ', self spec projectLabel.
	MetacelloNotification signal: ('Recording ', displayString, '...').
	originalLoader := self versionSpec loader.
	oldPolicy := loaderPolicy. 
	oldBypassProgress := MetacelloPlatform current bypassProgressBars.
	self loaderPolicy silently ifTrue: [ MetacelloPlatform current bypassProgressBars: true ].
	[	MetacelloPlatform current
			do:  [ | ans |
				self versionSpec loader: originalLoader recordingSpecLoader.
				ans := (self executeLoadFromArray: anArray) copy.
				MetacelloNotification signal: ('...finished ', self versionNumber printString).
				^ans ]
			displaying: 'Recording ', displayString ]
				ensure: [
					MetacelloPlatform current bypassProgressBars: oldBypassProgress.
					self versionSpec loader: originalLoader.
					loaderPolicy := oldPolicy ]
]

{ #category : 'private' }
MetacelloVersion >> executeLoadFromArray: anArray [

	| loader |
	loader := MetacelloMCVersionSpecLoader on: self spec.
	loader required: anArray.
	loaderPolicy ifNotNil: [ loader loaderPolicy: loaderPolicy ].
	^ loader load
]

{ #category : 'querying' }
MetacelloVersion >> expandToLoadableSpecNames: nameList [
	"Just like #resolveToLoadableSpecs:, but returns list of spec names instead of specs"
	
	^self spec expandToLoadableSpecNames: nameList
]

{ #category : 'actions' }
MetacelloVersion >> fetch [

	^self doFetchRequiredFromArray: self spec defaultPackageNames
]

{ #category : 'actions' }
MetacelloVersion >> fetch: required [

	^required fetchRequiredForMetacelloMCVersion: self
]

{ #category : 'private' }
MetacelloVersion >> fetchRequiredFromArray: anArray [
	| originalLoader displayString newLoader |
	originalLoader := self versionSpec loader.
	newLoader := originalLoader fetchingSpecLoader.
	displayString := newLoader actionLabel , self versionNumber printString , ' of ' , self spec projectLabel.
	MetacelloPlatform current
		do: [ 
			[ 
			self versionSpec loader: newLoader.
			MetacelloPlatform current
				useStackCacheDuring: [ :dict | ^ self executeLoadFromArray: anArray ]
				defaultDictionary: Dictionary new ] ensure: [ self versionSpec loader: originalLoader ] ]
		displaying: displayString
]

{ #category : 'querying' }
MetacelloVersion >> groups [
	"Answers the list of groups associated with this version"
	
	| groups |
	groups := OrderedCollection new.
	self spec projectDo: [:ignored | ] packageDo: [:ignored | ] groupDo: [:grp | groups add: grp ].
	^groups
]

{ #category : 'comparing' }
MetacelloVersion >> hash [

	^self versionNumber hash
]

{ #category : 'accessing' }
MetacelloVersion >> ignoreImage: aBool [ 
	"by default ignoreImage is false"
	
	self loaderPolicy ignoreImage: aBool
]

{ #category : 'accessing' }
MetacelloVersion >> importedVersions [

	^ importedVersions ifNil: [ importedVersions := #(  ) ]
]

{ #category : 'accessing' }
MetacelloVersion >> importedVersions: aCollection [

	importedVersions := aCollection
]

{ #category : 'testing' }
MetacelloVersion >> isSomethingLoaded [
	"at least one project or package has been loaded"
	
	^self spec isSomethingLoaded
]

{ #category : 'actions' }
MetacelloVersion >> load [

	^self doLoadRequiredFromArray: self spec defaultPackageNames
]

{ #category : 'actions' }
MetacelloVersion >> load: required [

	^required loadRequiredForMetacelloMCVersion: self
]

{ #category : 'private' }
MetacelloVersion >> loadRequiredFromArray: anArray [

	| displayString |
	displayString := 'Loading ', self versionNumber printString, ' of ', self spec projectLabel.
	MetacelloPlatform current
		do: [ ^self executeLoadFromArray: anArray ]
		displaying: displayString
]

{ #category : 'querying' }
MetacelloVersion >> loader [

	^self spec loader
]

{ #category : 'private' }
MetacelloVersion >> loader: aLoader [

	self versionSpec loader: aLoader
]

{ #category : 'accessing' }
MetacelloVersion >> loaderPolicy [

	^ loaderPolicy ifNil: [ loaderPolicy := MetacelloLoaderPolicy new ]
]

{ #category : 'accessing' }
MetacelloVersion >> loaderPolicy: anObject [
	loaderPolicy := anObject
]

{ #category : 'testing' }
MetacelloVersion >> name [

	^self versionString
]

{ #category : 'querying' }
MetacelloVersion >> packageAndProjectNamesToLoad: defaultList loader: aLoader [
	"Answer the list of packages and projects to be loaded: packages already loaded plust defaultList"

	| loadedPackageNames projectMap loadedProjectNames list |
	loadedPackageNames := ((self packages select: [ :pkg | pkg isPackageLoaded: aLoader ]) collect: [ :pkg | pkg name ]) asSet , defaultList.
	projectMap := Dictionary new.
	self projects do: [ :prj |
		prj className ifNotNil: [
			| coll loaded |
			coll := projectMap at: prj className ifAbsent: [
				        coll := OrderedCollection new.
				        projectMap at: prj className put: coll.
				        coll ].
			(loaded := prj loadedPackageNames: aLoader) isEmpty ifFalse: [ coll add: prj -> (loaded -> prj loadPackageList) ] ] ].
	loadedProjectNames := Set new.
	projectMap keysAndValuesDo: [ :prjClass :coll |
		coll size <= 1
			ifTrue: [ coll do: [ :assoc | loadedProjectNames add: assoc key name ] ]
			ifFalse: [ "multiple project references against the same configuration ... only count project as loaded
				 if there is an exact match for loaded projects...
				 See http://code.google.com/p/metacello/issues/detail?id=86"
				coll do: [ :assoc |
					| loaded packageList |
					loaded := assoc value key. "loaded packages from project"
					packageList := assoc value value. "loadlist for project"
					(packageList difference: loaded) isEmpty ifTrue: [ loadedProjectNames add: assoc key name ] ] ] ].
	list := loadedPackageNames , loadedProjectNames.
	list isEmpty ifTrue: [ ^ self spec defaultPackageNames ].
	^ list
]

{ #category : 'querying' }
MetacelloVersion >> packageNamed: aString [

	^self packageNamed: aString ifAbsent: [ ^nil ].
]

{ #category : 'querying' }
MetacelloVersion >> packageNamed: aString ifAbsent: aBlock [

	^ (self spec packageNamed: aString ifAbsent: [  ])
		  ifNil: [ aBlock value ]
		  ifNotNil: [ :pkg | pkg referencedSpec ]
]

{ #category : 'querying' }
MetacelloVersion >> packages [
	"Answers the list of packages associated with this version"

	| packages |
	packages := OrderedCollection new.
	self spec projectDo: [:ignored | ] packageDo: [:pkg | packages add: pkg ] groupDo: [:ignored | ].
	^packages
]

{ #category : 'querying' }
MetacelloVersion >> packagesForSpecNamed: aString [
	"resolves list of packages associated with the named spec.
	   If the spec is a packages, answer a list including only the package. #requires: and #includes:
			fields in the package are ignored.
	   If the spec is a project, answers an empty list.
	   If the spec is a group, answers the list of packages in the #includes: field of the group. 
			Groups in the #includes: field are expanded following the transitive closure on groups"
	"If there is no spec named <aString>, answers an empty list"

	^self packagesForSpecNamed: aString ifAbsent: [ ^#() ]
]

{ #category : 'querying' }
MetacelloVersion >> packagesForSpecNamed: aString ifAbsent: aBlock [
	"resolves list of packages associated with the named spec.
	   If the spec is a packages, answer a list including only the package. #requires: and #includes:
			fields in the package are ignored.
	   If the spec is a project, answers an empty list.
	   If the spec is a group, answers the list of packages in the #includes: field of the group. 
			Groups in the #includes: field are expanded following the transitive closure on groups"

	"If there is no spec named <aString>, aBlock is evaluated"

	^ (self spec packageNamed: aString ifAbsent: aBlock) resolveToPackagesIn: self spec visited: MetacelloVisitedPackages new
]

{ #category : 'printing' }
MetacelloVersion >> printOn: aStream [

	| label vs |
	(#( structural broken ) includes: self blessing) not ifTrue: [
		(vs := self versionStatus) == #somethingLoaded ifTrue: [ aStream nextPutAll: '<>' ].
		vs == #loadedMatchConstraints ifTrue: [ aStream nextPutAll: '>=' ].
		vs == #loadedToSpec ifTrue: [ aStream nextPut: $~ ] ].
	self versionNumber printOn: aStream.
	self basicSpec ifNotNil: [ (label := self projectLabel) isEmpty ifFalse: [ aStream nextPutAll: ' [' , label , ']' ] ]
]

{ #category : 'querying' }
MetacelloVersion >> project [
    ^ self basicSpec project
]

{ #category : 'accessing' }
MetacelloVersion >> projectLabel [
    ^ self basicSpec projectLabel
]

{ #category : 'querying' }
MetacelloVersion >> projects [
	"Answers the list of projects associated with this version"

	| projects |
	projects := OrderedCollection new.
	self spec projectDo: [:prj | projects add: prj projectReference ] packageDo: [:ignored | ] groupDo: [:ignored | ].
	^projects
]

{ #category : 'actions' }
MetacelloVersion >> record [

	^self doRecordRequiredFromArray: self spec defaultPackageNames
]

{ #category : 'actions' }
MetacelloVersion >> record: required [

	^required recordRequiredForMetacelloMCVersion: self
]

{ #category : 'querying' }
MetacelloVersion >> resolveToLoadableSpecs: nameList [
	"Resolves names in namelist to the  list of packages and projects in the version that would be loaded. Projects are 
	not traversed during the transitive closure. The scope is that of the version itself.
	   If the spec is a package, answer a list including the package and the transitive closure on 
			its #requires: and #includes: fields.
	   If the spec is a project, answer the project.
	   If the spec is a group, answers the list of packages in the #includes: field of the group. 
			Groups in the #includes: field are expanded following the transitive closure on groups"

	^self spec resolveToLoadableSpecs: nameList
]

{ #category : 'accessing' }
MetacelloVersion >> silently: aBool [ 
	"by default silently is false"
	
	self loaderPolicy silently: aBool
]

{ #category : 'accessing' }
MetacelloVersion >> spec [

	^spec
]

{ #category : 'accessing' }
MetacelloVersion >> spec: aMetacellVersionSpec [

	spec := aMetacellVersionSpec
]

{ #category : 'querying' }
MetacelloVersion >> timestamp [
    ^ self basicSpec timestamp value
]

{ #category : 'actions' }
MetacelloVersion >> unload [

	^self spec unload
]

{ #category : 'querying' }
MetacelloVersion >> versionKey [
	"version printString, without the versionStatus decorations, suitable for using as a unique key for the 
	 receiver in a dictionary"

	^ String streamContents: [ :aStream |
		  | label |
		  self versionNumber printOn: aStream.
		  self basicSpec ifNotNil: [ (label := self projectLabel) isEmpty ifFalse: [ aStream nextPutAll: ' [' , label , ']' ] ] ]
]

{ #category : 'querying' }
MetacelloVersion >> versionNumber [

	^versionNumber
]

{ #category : 'accessing' }
MetacelloVersion >> versionNumber: aVersionNumber [

	versionNumber := aVersionNumber
]

{ #category : 'querying' }
MetacelloVersion >> versionNumberFrom: aString [
    ^ self versionNumber class fromString: aString
]

{ #category : 'accessing' }
MetacelloVersion >> versionSpec [

	^self spec
]

{ #category : 'accessing' }
MetacelloVersion >> versionStatus [

	^ versionStatus ifNil: [ versionStatus := self computeVersionStatus ]
]

{ #category : 'accessing' }
MetacelloVersion >> versionStatus: aSymbol [

	versionStatus := aSymbol
]

{ #category : 'querying' }
MetacelloVersion >> versionString [

	^self versionNumber versionString
]

{ #category : 'comparing' }
MetacelloVersion >> ~> aMetacelloVersion [

	aMetacelloVersion species = self species ifFalse: [ ^false ].
	^self versionNumber ~> aMetacelloVersion versionNumber
]
