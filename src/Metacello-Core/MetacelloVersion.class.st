Class {
	#name : 'MetacelloVersion',
	#superclass : 'Magnitude',
	#instVars : [
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

{ #category : 'querying' }
MetacelloVersion >> defaultPackageNamesToLoad: defaultList [
	"Answer the list of packages and projects to be loaded: packages already loaded plust defaultList"
	
	^ self packageAndProjectNamesToLoad: defaultList loader: nil
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
MetacelloVersion >> doFetchRequiredFromArray: anArray withEngine: anEngine [

	^ self
		  doWithProgress: [
		  self fetchRequiredFromArray: anArray withEngine: anEngine ]
		  label: 'Fetching'
		  silently: anEngine silently
]

{ #category : 'private' }
MetacelloVersion >> doLoadRequiredFromArray: anArray withEngine: anEngine [

	^ self
		  doWithProgress: [
		  self loadRequiredFromArray: anArray withEngine: anEngine ]
		  label: 'Loading'
		  silently: anEngine silently
]

{ #category : 'private' }
MetacelloVersion >> doRecordRequiredFromArray: anArray withEngine: anEngine [

	^ self
		  doWithProgress: [
		  self recordRequiredFromArray: anArray withEngine: anEngine ]
		  label: 'Recording'
		  silently: anEngine silently
]

{ #category : 'private' }
MetacelloVersion >> doWithProgress: aBlock label: aLabel silently: silently [

	| displayString oldBypassProgress |
	displayString := ' ' , self versionNumber printString , ' of '
	                 , self spec projectLabel.
	MetacelloNotification signal: aLabel , displayString , '...'.
	oldBypassProgress := MetacelloPlatform current bypassProgressBars.

	silently ifTrue: [
		MetacelloPlatform current bypassProgressBars: true ].

	[
	MetacelloPlatform current
		do: [
			| result |
			result := aBlock value.

			MetacelloNotification signal:
				'...finished ' , self versionNumber printString.
			^ result ]
		displaying: aLabel , displayString ] ensure: [
		MetacelloPlatform current bypassProgressBars: oldBypassProgress ]
]

{ #category : 'private' }
MetacelloVersion >> fetchRequiredFromArray: anArray withEngine: anEngine [

	| directive |
	directive := self
		              recordRequiredFromArray:
		              (self defaultPackageNamesToLoad: anArray)
		              withEngine: anEngine.

	directive acceptVisitor: (MetacelloFetchTarget on: anEngine).
	^ directive
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
MetacelloVersion >> importedVersions [

	^ importedVersions ifNil: [ importedVersions := #(  ) ]
]

{ #category : 'accessing' }
MetacelloVersion >> importedVersions: aCollection [

	importedVersions := aCollection
]

{ #category : 'private' }
MetacelloVersion >> loadRequiredFromArray: anArray withEngine: anEngine [

	| directive |
	directive := self
		             fetchRequiredFromArray:
		             (self defaultPackageNamesToLoad: anArray)
		             withEngine: anEngine.
	directive acceptVisitor: (MetacelloLoadTarget on: anEngine).
	^ directive
]

{ #category : 'actions' }
MetacelloVersion >> loadUsingTarget: aTarget withEngine: anEngine [

	^self doLoadRequiredFromArray: self spec defaultPackageNames withEngine: anEngine
]

{ #category : 'querying' }
MetacelloVersion >> loader [

	^self spec loader
]

{ #category : 'testing' }
MetacelloVersion >> name [

	^self versionString
]

{ #category : 'querying' }
MetacelloVersion >> packageAndProjectNamesToLoad: defaultList loader: aLoader [
	"Answer the list of packages and projects to be loaded: packages already loaded plust defaultList"

	defaultList isEmpty ifTrue: [ ^ self spec defaultPackageNames ].
	^ defaultList
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

	| label |
	self versionNumber printOn: aStream.
	self basicSpec ifNotNil: [
		(label := self projectLabel) isEmpty ifFalse: [
			aStream nextPutAll: ' [' , label , ']' ] ]
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

{ #category : 'private' }
MetacelloVersion >> recordRequiredFromArray: anArray withEngine: anEngine [

	| target |
	target := MetacelloRecordTarget on: anEngine.
	target required: anArray.

	self spec acceptVisitor: target.

	^ target specBuilder root
]

{ #category : 'actions' }
MetacelloVersion >> recordWithEngine: anEngine [

	^self doRecordRequiredFromArray: self spec defaultPackageNames withEngine: anEngine
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
