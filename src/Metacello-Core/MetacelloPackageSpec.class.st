Class {
	#name : 'MetacelloPackageSpec',
	#superclass : 'MetacelloLoadableSpec',
	#instVars : [
		'requires',
		'project',
		'name',
		'repositories',
		'preLoadDoIt',
		'postLoadDoIt',
		'strict'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'instance creation' }
MetacelloPackageSpec class >> project: aMetacelloProject [

	^ self new
		  project: aMetacelloProject;
		  yourself
]

{ #category : 'visiting' }
MetacelloPackageSpec >> acceptVisitor: aVisitor [

	^ aVisitor visitPackageSpec: self
]

{ #category : 'loading' }
MetacelloPackageSpec >> ensureLoadedForDevelopmentUsing: aTarget [
	"noop"

	
]

{ #category : 'loading' }
MetacelloPackageSpec >> ensureLoadedForDevelopmentUsing: aTarget ithEngine: anEngine [
	"noop"

	
]

{ #category : 'accessing' }
MetacelloPackageSpec >> getRepositories [
    "raw access to iv"

    ^ repositories
]

{ #category : 'private' }
MetacelloPackageSpec >> includeSpecNamesForPackageOrdering: aVersionSpec [

	^ self specs: self includesForPackageOrdering forPackageOrdering: aVersionSpec
]

{ #category : 'private' }
MetacelloPackageSpec >> includesForPackageOrdering [
  ^ self includes
]

{ #category : 'accessing' }
MetacelloPackageSpec >> info [
	"test compatibility method"

	^self
]

{ #category : 'testing' }
MetacelloPackageSpec >> isPackageSpec [

	^ true
]

{ #category : 'merging' }
MetacelloPackageSpec >> mergeMap [

	| map |
	map := super mergeMap.
	map at: #preLoadDoIt put: preLoadDoIt.
	map at: #postLoadDoIt put: postLoadDoIt.
	^ map
]

{ #category : 'merging' }
MetacelloPackageSpec >> mergeSpec: anotherSpec [

	super mergeSpec: anotherSpec.
	anotherSpec requires ifNotEmpty: [ :anotherRequires | self requires: self requires , anotherRequires ].
	self repositories: (self repositories mergeSpec: anotherSpec repositories).

	anotherSpec name ifNotNil: [ self name: anotherSpec name ]
]

{ #category : 'querying' }
MetacelloPackageSpec >> name [

	^name
]

{ #category : 'accessing' }
MetacelloPackageSpec >> name: aString [

	name := aString trim
]

{ #category : 'loading' }
MetacelloPackageSpec >> packageSpecsInLoadOrder [

	^ { self }
]

{ #category : 'copying' }
MetacelloPackageSpec >> postCopy [

	super postCopy.
	repositories := repositories copy.
	requires := requires copy
]

{ #category : 'querying' }
MetacelloPackageSpec >> postLoadDoIt [

	^postLoadDoIt
]

{ #category : 'loading' }
MetacelloPackageSpec >> postLoadDoIt: aSymbol [

	self validateDoItSelector: aSymbol.
	postLoadDoIt := aSymbol
]

{ #category : 'querying' }
MetacelloPackageSpec >> preLoadDoIt [

	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloPackageSpec >> preLoadDoIt: aSymbol [

	self validateDoItSelector: aSymbol.
	preLoadDoIt := aSymbol
]

{ #category : 'accessing' }
MetacelloPackageSpec >> project [

	^ project
]

{ #category : 'accessing' }
MetacelloPackageSpec >> project: aMetacelloProject [

	project := aMetacelloProject
]

{ #category : 'querying' }
MetacelloPackageSpec >> repositories [

	^ repositories ifNil: [ repositories := MetacelloRepositoriesSpec new ]
]

{ #category : 'querying' }
MetacelloPackageSpec >> repositories: anObject [
	repositories := anObject
]

{ #category : 'accessing' }
MetacelloPackageSpec >> repository: aStringOrMetacelloRepositorySpec [

	self repositories repository: aStringOrMetacelloRepositorySpec
]

{ #category : 'accessing' }
MetacelloPackageSpec >> repository: aString username: username password: password [

	self repositories repository: aString username: username password: password
]

{ #category : 'loading' }
MetacelloPackageSpec >> repositoryDescriptions [
    ^ self repositorySpecs collect: [ :repoSpec | repoSpec description ]
]

{ #category : 'loading' }
MetacelloPackageSpec >> repositorySpecs [

	^self repositories repositorySpecs
]

{ #category : 'private' }
MetacelloPackageSpec >> requiredSpecNamesForPackageOrdering: aVersionSpec [

	^ (self specs: self requires forPackageOrdering: aVersionSpec) collect: [ :spec | spec name ]
]

{ #category : 'querying' }
MetacelloPackageSpec >> requires [

	^ requires ifNil: [ requires := #(  ) ]
]

{ #category : 'accessing' }
MetacelloPackageSpec >> requires: aCollection [

	requires := aCollection isString
		            ifTrue: [ { aCollection } ]
		            ifFalse: [ aCollection asArray ]
]

{ #category : 'private' }
MetacelloPackageSpec >> resolveToPackagesAndProjectsIn: aVersionSpec [

	^ { self }
]

{ #category : 'private' }
MetacelloPackageSpec >> resolveToPackagesIn: aVersionSpec visited: visited [

	^{ self }
]

{ #category : 'asserting' }
MetacelloPackageSpec >> shouldLoadStrict [

	^ strict ifNil: [ project ifNotNil: [ :p | p strict ] ]
]

{ #category : 'private' }
MetacelloPackageSpec >> specs: specNames forPackageOrdering: aVersionSpec [

	| packageOrProjectSpecs |
	packageOrProjectSpecs := Set new.
	specNames do: [ :specName |
			| spec |
			"get project/group/package specs"
			spec := aVersionSpec packageNamed: specName ifAbsent: [
					        MetacelloNameNotDefinedError signal:
						        'project group, or package named: ' , specName printString , ' not found when used in requires: or includes: field of package: '
						        , self name printString , ' of ' , aVersionSpec projectLabel , '.'.
					        nil "return nil if resumed" ].
			spec ifNotNil: [ packageOrProjectSpecs addAll: (spec resolveSpecsForPackageOrdering: aVersionSpec) ] ].
	^ packageOrProjectSpecs
]

{ #category : 'accessing' }
MetacelloPackageSpec >> strict: aBoolean [ 
	strict := aBoolean
]

{ #category : 'accessing' }
MetacelloPackageSpec >> workingCopy [
	"Answer a working copy, or nil if the package is not loaded."

	| wcs |
	wcs := MCWorkingCopy allWorkingCopies select: [ :each |
		       each packageName = self name ].
	wcs isEmpty ifTrue: [ ^ nil ].
	^ wcs detectMax: [ :ea | ea package name size ]
]
