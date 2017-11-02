Class {
	#name : #MetacelloVersion,
	#superclass : #Magnitude,
	#instVars : [
		'spec',
		'versionNumber',
		'importedVersions',
		'versionStatus'
	],
	#category : 'Metacello-Core-Model'
}

{ #category : #'instance creation' }
MetacelloVersion class >> fromSpec: aMetacelloVersionSpec [
    ^ (self new versionNumber: aMetacelloVersionSpec versionNumber)
        spec: aMetacelloVersionSpec;
        yourself
]

{ #category : #'instance creation' }
MetacelloVersion class >> fromString: aString [

	^self new versionNumber: aString asMetacelloVersionNumber
]

{ #category : #comparing }
MetacelloVersion >> < aMetacelloVersion [

	aMetacelloVersion species = self species ifFalse: [ ^false ].
	^self versionNumber < aMetacelloVersion versionNumber
]

{ #category : #comparing }
MetacelloVersion >> = aMetacelloVersion [

	aMetacelloVersion species = self species ifFalse: [ ^false ].
	^self versionNumber = aMetacelloVersion versionNumber
]

{ #category : #querying }
MetacelloVersion >> author [

	^self spec author value
]

{ #category : #accessing }
MetacelloVersion >> basicSpec [
    ^ self spec
]

{ #category : #querying }
MetacelloVersion >> blessing [
    ^ self basicSpec blessing value
]

{ #category : #private }
MetacelloVersion >> computeVersionStatus [
    ^ #'noStatus'
]

{ #category : #querying }
MetacelloVersion >> description [
    ^ self basicSpec description value
]

{ #category : #querying }
MetacelloVersion >> difference: aMetacelloVersion [
	"Return a a dictionary of additions, removals and modifications"
	
	^self spec difference: aMetacelloVersion spec
]

{ #category : #querying }
MetacelloVersion >> expandToLoadableSpecNames: nameList [
	"Just like #resolveToLoadableSpecs:, but returns list of spec names instead of specs"
	
	^self spec expandToLoadableSpecNames: nameList
]

{ #category : #querying }
MetacelloVersion >> groups [
	"Answers the list of groups associated with this version"
	
	| groups |
	groups := OrderedCollection new.
	self spec projectDo: [:ignored | ] packageDo: [:ignored | ] groupDo: [:grp | groups add: grp ].
	^groups
]

{ #category : #comparing }
MetacelloVersion >> hash [

	^self versionNumber hash
]

{ #category : #accessing }
MetacelloVersion >> importedVersions [

	importedVersions == nil ifTrue: [ importedVersions := #() ].
	^importedVersions
]

{ #category : #accessing }
MetacelloVersion >> importedVersions: aCollection [

	importedVersions := aCollection
]

{ #category : #testing }
MetacelloVersion >> isAllLoadedToSpec [
	"all projects and packages are loaded and match specification"
	
	^self spec isAllLoadedToSpec
]

{ #category : #testing }
MetacelloVersion >> isLoadedMatchConstraints [
	"all loaded projects and packages match constraints"
	
	^self spec isLoadedMatchConstraints
]

{ #category : #testing }
MetacelloVersion >> isLoadedToSpec [
	"all loaded projects and packages match specifications"
	
	^self spec isLoadedToSpec
]

{ #category : #testing }
MetacelloVersion >> isPossibleBaseline [

	^self spec isPossibleBaseline
]

{ #category : #testing }
MetacelloVersion >> isSomethingLoaded [
	"at least one project or package has been loaded"
	
	^self spec isSomethingLoaded
]

{ #category : #actions }
MetacelloVersion >> load [

	^self subclassResponsibility
]

{ #category : #actions }
MetacelloVersion >> load: required [

	^self subclassResponsibility
]

{ #category : #querying }
MetacelloVersion >> loader [

	^self spec loader
]

{ #category : #testing }
MetacelloVersion >> name [

	^self versionString
]

{ #category : #querying }
MetacelloVersion >> packageNamed: aString [

	^self packageNamed: aString ifAbsent: [ ^nil ].
]

{ #category : #querying }
MetacelloVersion >> packageNamed: aString ifAbsent: aBlock [

	| pkg |
	(pkg := self spec packageNamed: aString ifAbsent: []) == nil
		ifTrue: [ ^aBlock value ].
	^pkg referencedSpec
]

{ #category : #printing }
MetacelloVersion >> printOn: aStream [
    | label vs |
    (#(structural broken) includes: self blessing) not
        ifTrue: [ 
            (vs := self versionStatus) == #'somethingLoaded'
                ifTrue: [ aStream nextPutAll: '<>' ].
            vs == #'loadedMatchConstraints'
                ifTrue: [ aStream nextPutAll: '>=' ].
            vs == #'loadedToSpec'
                ifTrue: [ aStream nextPut: $~ ] ].
    self versionNumber printOn: aStream.
    self basicSpec ~~ nil
        ifTrue: [ 
            (label := self projectLabel) isEmpty
                ifFalse: [ aStream nextPutAll: ' [' , label , ']' ] ]
]

{ #category : #querying }
MetacelloVersion >> project [
    ^ self basicSpec project
]

{ #category : #accessing }
MetacelloVersion >> projectLabel [
    ^ self basicSpec projectLabel
]

{ #category : #querying }
MetacelloVersion >> projects [
	"Answers the list of projects associated with this version"

	| projects |
	projects := OrderedCollection new.
	self spec projectDo: [:prj | projects add: prj projectReference ] packageDo: [:ignored | ] groupDo: [:ignored | ].
	^projects
]

{ #category : #querying }
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

{ #category : #accessing }
MetacelloVersion >> spec [

	^spec
]

{ #category : #accessing }
MetacelloVersion >> spec: aMetacellVersionSpec [

	spec := aMetacellVersionSpec
]

{ #category : #querying }
MetacelloVersion >> timestamp [
    ^ self basicSpec timestamp value
]

{ #category : #actions }
MetacelloVersion >> unload [

	^self spec unload
]

{ #category : #querying }
MetacelloVersion >> versionKey [
    "version printString, without the versionStatus decorations, suitable for using as a unique key for the 
	 receiver in a dictionary"

    ^ String
        streamContents: [ :aStream | 
            | label |
            self versionNumber printOn: aStream.
            self basicSpec ~~ nil
                ifTrue: [ 
                    (label := self projectLabel) isEmpty
                        ifFalse: [ aStream nextPutAll: ' [' , label , ']' ] ] ]
]

{ #category : #querying }
MetacelloVersion >> versionNumber [

	^versionNumber
]

{ #category : #accessing }
MetacelloVersion >> versionNumber: aVersionNumber [

	versionNumber := aVersionNumber
]

{ #category : #querying }
MetacelloVersion >> versionNumberFrom: aString [
    ^ self versionNumber class fromString: aString
]

{ #category : #accessing }
MetacelloVersion >> versionSpec [

	^self spec
]

{ #category : #accessing }
MetacelloVersion >> versionStatus [

	versionStatus == nil ifTrue: [ versionStatus := self computeVersionStatus].
	^versionStatus
]

{ #category : #accessing }
MetacelloVersion >> versionStatus: aSymbol [

	versionStatus := aSymbol
]

{ #category : #querying }
MetacelloVersion >> versionString [

	^self versionNumber versionString
]

{ #category : #comparing }
MetacelloVersion >> ~> aMetacelloVersion [

	aMetacelloVersion species = self species ifFalse: [ ^false ].
	^self versionNumber ~> aMetacelloVersion versionNumber
]
