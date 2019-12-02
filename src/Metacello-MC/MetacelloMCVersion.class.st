Class {
	#name : #MetacelloMCVersion,
	#superclass : #MetacelloVersion,
	#instVars : [
		'loaderPolicy'
	],
	#category : #'Metacello-MC-Model'
}

{ #category : #querying }
MetacelloMCVersion >> allPackagesForSpecNamed: aStringOrArray [
	"resolves list of packages associated with the named spec.
	   If the spec is a packages, answer a list including the package and the transitive closure on 
			its #requires: and #includes: fields.
	   If the spec is a project, answer a list of the packages associated with the project,
			following the transitive closure on packages reachable starting with the #loads: field.
	   If the spec is a group, answers the list of packages in the #includes: field of the group. 
			Groups in the #includes: field are expanded following the transitive closure on groups"
	"In essence, this query answers the list of all packages that would be loaded if the package 
		named <aString> were loaded."
	"If there is no spec named <aString>, answers an empty list"

	^self allPackagesForSpecNamed: aStringOrArray ifAbsent: [ ^#() ]
]

{ #category : #querying }
MetacelloMCVersion >> allPackagesForSpecNamed: aStringOrArray ifAbsent: aBlock [
	"resolves list of packages associated with the named spec.
	   If the spec is a packages, answer a list including the package and the transitive closure on 
			its #requires: and #includes: fields.
	   If the spec is a project, answer a list of the packages associated with the project,
			following the transitive closure on packages reachable starting with the #loads: field.
	   If the spec is a group, answers the list of packages in the #includes: field of the group. 
			Groups in the #includes: field are expanded following the transitive closure on groups"
	"In essence, this query answers the list of all packages that would be loaded if the package 
		named <aString> were loaded."
	"If there is no spec named <aString>, aBlock is evaluated"

	^aStringOrArray 
		resolvePackageSpecsNamedForMetacelloMCVersion: self 
		visited: MetacelloVisitedPackages new 
		ifAbsent: aBlock
]

{ #category : #private }
MetacelloMCVersion >> allPackagesForSpecs: pkgSpecs visited: visited [

 
	| coll |
	coll := Dictionary new.
	pkgSpecs
		do: [:pkgSpec | (pkgSpec resolveToAllPackagesIn: self spec visited: visited)
				do: [:pkg | coll at: pkg name put: pkg ]].
	^ coll values asOrderedCollection
]

{ #category : #accessing }
MetacelloMCVersion >> cacheRepository: repository [ 
	"by default cacheRepository is an MCDictionaryRepository"
	
	self loaderPolicy cacheRepository: repository
]

{ #category : #private }
MetacelloMCVersion >> computeVersionStatus [
	"
		#allLoadedToSpec - all projects and packages are loaded and match specification
		#loadedToSpec - all loaded projects and packages match specifications (at least one package loaded)
		#loadedMatchConstraints - all loaded projects and packages match constraints (at least one package loaded)
		#somethingLoaded - at least one package loaded
	"

	self spec computeVersionStatus: [ :status | ^ status ].
	^ #noStatus
]

{ #category : #querying }
MetacelloMCVersion >> currentlyLoadedClassesInVersion [

	^self spec currentlyLoadedClassesInVersion
]

{ #category : #querying }
MetacelloMCVersion >> currentlyLoadedExtensionClassesInVersion [

	^self spec currentlyLoadedExtensionClassesInVersion
]

{ #category : #querying }
MetacelloMCVersion >> defaultPackageNamesToLoad [
	"Answer the list of packages and projects to be loaded --> packages already loaded"
	
	^ self defaultPackageNamesToLoad: self spec defaultPackageNames
]

{ #category : #querying }
MetacelloMCVersion >> defaultPackageNamesToLoad: defaultList [
	"Answer the list of packages and projects to be loaded: packages already loaded plust defaultList"
	
	^ self packageAndProjectNamesToLoad: defaultList loader: self loader
]

{ #category : #private }
MetacelloMCVersion >> doFetchRequiredFromArray: anArray [

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

{ #category : #private }
MetacelloMCVersion >> doLoadRequiredFromArray: anArray [

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

{ #category : #private }
MetacelloMCVersion >> doRecordRequiredFromArray: anArray [

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

{ #category : #private }
MetacelloMCVersion >> executeLoadFromArray: anArray [

	| loader |
	loader := MetacelloMCVersionSpecLoader on: self spec.
	loader required: anArray.
	loaderPolicy notNil ifTrue: [ loader loaderPolicy: loaderPolicy ].
	^loader load

]

{ #category : #actions }
MetacelloMCVersion >> fetch [

	^self doFetchRequiredFromArray: self spec defaultPackageNames
]

{ #category : #actions }
MetacelloMCVersion >> fetch: required [

	^required fetchRequiredForMetacelloMCVersion: self
]

{ #category : #private }
MetacelloMCVersion >> fetchRequiredFromArray: anArray [
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

{ #category : #accessing }
MetacelloMCVersion >> ignoreImage: aBool [ 
	"by default ignoreImage is false"
	
	self loaderPolicy ignoreImage: aBool
]

{ #category : #actions }
MetacelloMCVersion >> load [

	^self doLoadRequiredFromArray: self spec defaultPackageNames
]

{ #category : #actions }
MetacelloMCVersion >> load: required [

	^required loadRequiredForMetacelloMCVersion: self
]

{ #category : #private }
MetacelloMCVersion >> loadRequiredFromArray: anArray [

	| displayString |
	displayString := 'Loading ', self versionNumber printString, ' of ', self spec projectLabel.
	MetacelloPlatform current
		do: [ ^self executeLoadFromArray: anArray ]
		displaying: displayString
]

{ #category : #private }
MetacelloMCVersion >> loader: aLoader [

	self versionSpec loader: aLoader
]

{ #category : #accessing }
MetacelloMCVersion >> loaderPolicy [

	loaderPolicy == nil ifTrue: [ loaderPolicy := MetacelloLoaderPolicy new ].
	^ loaderPolicy
]

{ #category : #accessing }
MetacelloMCVersion >> loaderPolicy: anObject [
	loaderPolicy := anObject
]

{ #category : #querying }
MetacelloMCVersion >> packageAndProjectNamesToLoad: defaultList loader: aLoader [
	"Answer the list of packages and projects to be loaded: packages already loaded plust defaultList"
	
	| loadedPackageNames projectMap loadedProjectNames list |
	loadedPackageNames := ((self packages select: [:pkg | pkg isPackageLoaded: aLoader ]) 
		collect: [:pkg | pkg name ]) asSet, 
			defaultList.
	projectMap := Dictionary new.
	self  projects do: [:prj | 
		prj className ~~ nil
			ifTrue: [ | coll loaded |
				coll := projectMap at: prj className ifAbsent: [
					coll := OrderedCollection new.
					projectMap at: prj className put: coll.
					coll].
				(loaded := prj loadedPackageNames: aLoader) isEmpty 
					ifFalse: [  coll add: prj -> (loaded -> prj loadPackageList) ]]].
	loadedProjectNames := Set new.
	projectMap keysAndValuesDo: [:prjClass :coll |
		coll size <= 1
			ifTrue: [ coll do: [:assoc | loadedProjectNames add: assoc key name ]]
			ifFalse: [ 
				"multiple project references against the same configuration ... only count project as loaded
				 if there is an exact match for loaded projects...
				 See http://code.google.com/p/metacello/issues/detail?id=86"
				coll do: [:assoc | | loaded packageList |
					loaded := assoc value key. "loaded packages from project"
					packageList := assoc value value. "loadlist for project"
					(packageList difference: loaded) isEmpty
						ifTrue: [ loadedProjectNames add: assoc key name ]]]].
	list := loadedPackageNames, loadedProjectNames.
	list isEmpty ifTrue: [ ^self spec defaultPackageNames].
	^list
]

{ #category : #querying }
MetacelloMCVersion >> packages [
	"Answers the list of packages associated with this version"

	| packages |
	packages := OrderedCollection new.
	self spec projectDo: [:ignored | ] packageDo: [:pkg | packages add: pkg ] groupDo: [:ignored | ].
	^packages
]

{ #category : #querying }
MetacelloMCVersion >> packagesForSpecNamed: aString [
	"resolves list of packages associated with the named spec.
	   If the spec is a packages, answer a list including only the package. #requires: and #includes:
			fields in the package are ignored.
	   If the spec is a project, answers an empty list.
	   If the spec is a group, answers the list of packages in the #includes: field of the group. 
			Groups in the #includes: field are expanded following the transitive closure on groups"
	"If there is no spec named <aString>, answers an empty list"

	^self packagesForSpecNamed: aString ifAbsent: [ ^#() ]
]

{ #category : #querying }
MetacelloMCVersion >> packagesForSpecNamed: aString ifAbsent: aBlock [
	"resolves list of packages associated with the named spec.
	   If the spec is a packages, answer a list including only the package. #requires: and #includes:
			fields in the package are ignored.
	   If the spec is a project, answers an empty list.
	   If the spec is a group, answers the list of packages in the #includes: field of the group. 
			Groups in the #includes: field are expanded following the transitive closure on groups"
	"If there is no spec named <aString>, aBlock is evaluated"

	| pkgSpec |
	pkgSpec := self spec packageNamed: aString ifAbsent: aBlock.
	^pkgSpec resolveToPackagesIn: self spec visited: MetacelloVisitedPackages new
]

{ #category : #actions }
MetacelloMCVersion >> record [

	^self doRecordRequiredFromArray: self spec defaultPackageNames
]

{ #category : #actions }
MetacelloMCVersion >> record: required [

	^required recordRequiredForMetacelloMCVersion: self
]

{ #category : #accessing }
MetacelloMCVersion >> repositoryOverrides: repositoriesCollection [ 

	self loaderPolicy overrideRepositories: repositoriesCollection
]

{ #category : #accessing }
MetacelloMCVersion >> silently: aBool [ 
	"by default silently is false"
	
	self loaderPolicy silently: aBool
]
