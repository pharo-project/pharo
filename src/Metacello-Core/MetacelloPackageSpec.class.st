Class {
	#name : 'MetacelloPackageSpec',
	#superclass : 'MetacelloAbstractPackageSpec',
	#instVars : [
		'file',
		'repositories',
		'goferPackage',
		'preLoadDoIt',
		'postLoadDoIt'
	],
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'visiting' }
MetacelloPackageSpec >> acceptVisitor: aVisitor [

	^ aVisitor visitPackageSpec: self
]

{ #category : 'printing' }
MetacelloPackageSpec >> configMethodBodyOn: aStream hasName: hasName indent: indent [

	| hasFile hasRepositories hasPreLoadDoIt hasPostLoadDoIt hasRequiresOrIncludesOrAnswers |
	hasFile := file isNotNil.
	hasRepositories := self repositorySpecs size > 0.
	hasPreLoadDoIt := self getPreLoadDoIt isNotNil.
	hasPostLoadDoIt := self getPostLoadDoIt isNotNil.
	hasRequiresOrIncludesOrAnswers := (self requires isEmpty and: [ self includes isEmpty and: [ self answers isEmpty ] ]) not.
	hasRequiresOrIncludesOrAnswers ifTrue: [
		self
			configMethodBodyOn: aStream
			hasName: hasName
			cascading: hasFile | hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt
			indent: indent ].
	self
		configMethodOn: aStream
		for: file
		selector: 'file: '
		cascading: hasName | hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers
		cascade: hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt
		indent: indent.
	hasRepositories ifTrue: [
		self repositorySpecs size > 1
			ifTrue: [
				hasName | hasFile | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers ifTrue: [
					aStream
						cr;
						tab: indent ].
				aStream
					nextPutAll: 'repositories: [';
					cr;
					tab: indent + 1;
					nextPutAll: 'spec';
					cr.
				self repositories configMethodCascadeOn: aStream indent: indent + 1.
				aStream nextPutAll: ' ]' ]
			ifFalse: [
				hasName | hasFile | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers ifTrue: [
					aStream
						cr;
						tab: indent ].
				self repositories configMethodCascadeOn: aStream indent: indent ].
		hasPreLoadDoIt | hasPostLoadDoIt ifTrue: [ aStream nextPut: $; ] ].
	self
		configMethodOn: aStream
		for: self getPreLoadDoIt
		selector: 'preLoadDoIt: '
		cascading: hasName | hasFile | hasRepositories | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers
		cascade: hasPostLoadDoIt
		indent: indent.
	self
		configMethodOn: aStream
		for: self getPostLoadDoIt
		selector: 'postLoadDoIt: '
		cascading: hasName | hasFile | hasRepositories | hasPreLoadDoIt | hasRequiresOrIncludesOrAnswers
		cascade: false
		indent: indent.
	aStream nextPut: $.
]

{ #category : 'printing' }
MetacelloPackageSpec >> configMethodCascadeOn: aStream member: aMember last: lastCascade indent: indent [

	aMember methodUpdateSelector  == #remove:
		ifTrue: [ aStream  nextPutAll: 'removePackage: ', self name printString ]
		ifFalse: [ self configShortCutMethodBodyOn: aStream member: aMember indent: indent ].
	lastCascade
		ifTrue: [ aStream nextPut: $. ]
		ifFalse: [ aStream nextPut: $;; cr ]
]

{ #category : 'printing' }
MetacelloPackageSpec >> configMethodOn: aStream indent: indent [

	| hasRepositories hasPreLoadDoIt hasPostLoadDoIt hasRequiresOrIncludesOrAnswers hasFile |
	hasFile := file isNotNil.
	hasRepositories := self repositorySpecs size > 0.
	hasPreLoadDoIt := self getPreLoadDoIt isNotNil.
	hasPostLoadDoIt := self getPostLoadDoIt isNotNil.
	hasRequiresOrIncludesOrAnswers := (self requires isEmpty and: [ self includes isEmpty and: [ self answers isEmpty ] ]) not.
	aStream
		tab: indent;
		nextPutAll: 'spec '.
	hasFile | hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers
		ifTrue: [
			aStream
				cr;
				tab: indent + 1;
				nextPutAll: 'name: ' , self name printString;
				nextPut: $;.
			self configMethodBodyOn: aStream hasName: true indent: indent + 1 ]
		ifFalse: [ aStream nextPutAll: 'name: ' , self name printString ]
]

{ #category : 'printing' }
MetacelloPackageSpec >> configShortCutMethodBodyOn: aStream member: aMember indent: indent [

	| hasFile hasRepositories hasPreLoadDoIt hasPostLoadDoIt hasRequiresOrIncludesOrAnswers |
	hasFile := file isNotNil.
	hasRepositories := self repositorySpecs size > 0.
	hasPreLoadDoIt := self getPreLoadDoIt isNotNil.
	hasPostLoadDoIt := self getPostLoadDoIt isNotNil.
	hasRequiresOrIncludesOrAnswers := (self requires isEmpty and: [ self includes isEmpty and: [ self answers isEmpty ] ]) not.
	hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers ifTrue: [
		aStream
			nextPutAll: 'package: ' , self name printString , ' ';
			nextPutAll: aMember methodUpdateSelector asString , ' [';
			cr.
		aStream
			tab: indent + 1;
			nextPutAll: 'spec '.
		self configMethodBodyOn: aStream hasName: false indent: indent + 2.
		aStream nextPutAll: ' ]'.
		^ self ].
	aStream nextPutAll: 'package: ' , self name printString.
	hasFile ifTrue: [ aStream nextPutAll: ' with: ' , file printString ]
]

{ #category : 'loading' }
MetacelloPackageSpec >> ensureLoadedForDevelopmentUsing: aTarget [
	"noop"

	
]

{ #category : 'loading' }
MetacelloPackageSpec >> ensureLoadedForDevelopmentUsing: aTarget ithEngine: anEngine [
	"noop"

	
]

{ #category : 'loading' }
MetacelloPackageSpec >> ensureLoadedForDevelopmentUsing: aTarget withEngine: anEngine [
	"noop"

	
]

{ #category : 'private' }
MetacelloPackageSpec >> extractNameFromFile [

	^ file
]

{ #category : 'querying' }
MetacelloPackageSpec >> file [

	file ifNil: [ ^ self name ].
	^ file
]

{ #category : 'accessing' }
MetacelloPackageSpec >> file: aString [

	file := aString
]

{ #category : 'loading' }
MetacelloPackageSpec >> forceLoadingWithEngine: anEngine [

	anEngine doLoadProjectSpecFrom: self
]

{ #category : 'accessing' }
MetacelloPackageSpec >> getFile [
	"raw access to iv"
	
	^file
]

{ #category : 'accessing' }
MetacelloPackageSpec >> getName [
    "raw access to iv"

    ^ name
]

{ #category : 'querying' }
MetacelloPackageSpec >> getPostLoadDoIt [

	^postLoadDoIt
]

{ #category : 'querying' }
MetacelloPackageSpec >> getPreLoadDoIt [

	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloPackageSpec >> getRepositories [
    "raw access to iv"

    ^ repositories
]

{ #category : 'testing' }
MetacelloPackageSpec >> hasRepository [
    ^ self repositorySpecs notEmpty
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

{ #category : 'merging' }
MetacelloPackageSpec >> mergeMap [

	| map |
	map := super mergeMap.
	map at: #file put: file.
	map at: #repositories put: self repositories.
	map at: #preLoadDoIt put: preLoadDoIt.
	map at: #postLoadDoIt put: postLoadDoIt.
	^map
]

{ #category : 'merging' }
MetacelloPackageSpec >> mergeSpec: anotherSpec [

	| newSpec map anotherRepositories |
	newSpec := super mergeSpec: anotherSpec.
	map := anotherSpec mergeMap.
	(anotherRepositories := map at: #repositories) notEmpty
		ifTrue: [ 
			newSpec 
				repositories: (self repositories isEmpty
					ifTrue: [ anotherRepositories ]
					ifFalse: [ self repositories mergeSpec: anotherRepositories ]) ].
	^newSpec
]

{ #category : 'querying' }
MetacelloPackageSpec >> name [

	^ name ifNil: [ name := self extractNameFromFile ]
]

{ #category : 'merging' }
MetacelloPackageSpec >> nonOverridable [

	^super nonOverridable, #( repositories)
]

{ #category : 'loading' }
MetacelloPackageSpec >> packageSpecsInLoadOrderWithEngine: anObject [

	^ { self }
]

{ #category : 'copying' }
MetacelloPackageSpec >> postCopy [

	super postCopy.
	goferPackage := nil.
	repositories := repositories copy.
]

{ #category : 'querying' }
MetacelloPackageSpec >> postLoadDoIt [

	^postLoadDoIt
]

{ #category : 'loading' }
MetacelloPackageSpec >> postLoadDoIt: anObject [

	anObject setPostLoadDoItInMetacelloSpec: self
]

{ #category : 'querying' }
MetacelloPackageSpec >> preLoadDoIt [

	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloPackageSpec >> preLoadDoIt: anObject [

	anObject setPreLoadDoItInMetacelloSpec: self
]

{ #category : 'visiting' }
MetacelloPackageSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	packageBlock value: self
]

{ #category : 'querying' }
MetacelloPackageSpec >> repositories [

	^ repositories ifNil: [ repositories := self project repositoriesSpec ]
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

	^self repositories map values
]

{ #category : 'as yet unclassified' }
MetacelloPackageSpec >> requiredPackageSpecsWithEngine: anEngine [

	| specs requiredSpec |
	self project ifNil: [ ^ #(  ) ].
	specs := self project version spec
		         packageSpecsInLoadOrderWithEngine: anEngine.
	^ self requires flatCollect: [ :required |
		  requiredSpec := specs detect: [ :e | e name = required ].
		  requiredSpec packageSpecsInLoadOrderWithEngine: anEngine ]
]

{ #category : 'private' }
MetacelloPackageSpec >> resolveToPackagesIn: aVersionSpec visited: visited [

	^{ self }
]

{ #category : 'fetching' }
MetacelloPackageSpec >> searchCacheRepositoryForPackage: searchBlock [
  "evaluate the <searchBlock> if you want to search for the package in a local package cache"

  "for standard mcz repositories the answer is YES!"

  searchBlock value
]

{ #category : 'accessing' }
MetacelloPackageSpec >> setPostLoadDoIt: aSymbol [

	postLoadDoIt := aSymbol
]

{ #category : 'accessing' }
MetacelloPackageSpec >> setPreLoadDoIt: aSymbol [

	preLoadDoIt := aSymbol
]
