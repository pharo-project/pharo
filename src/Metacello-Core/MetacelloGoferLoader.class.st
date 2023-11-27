Class {
	#name : 'MetacelloGoferLoader',
	#superclass : 'Object',
	#category : 'Metacello-Core-Gofer',
	#package : 'Metacello-Core',
	#tag : 'Gofer'
}

{ #category : 'as yet unclassified' }
MetacelloGoferLoader >> loadPackageDirective: aMetacelloPackageLoadDirective [

	| resolvedReference goferLoad repo gofer monticelloRepositories |
	gofer := Gofer new.
	monticelloRepositories := self repositoriesFrom:
		                          aMetacelloPackageLoadDirective
			                          externalReference.
	monticelloRepositories do: [ :repo | gofer repository: repo ].
	resolvedReference := self
		                     resolveRecordingPackageSpecReference:
		                     aMetacelloPackageLoadDirective spec
		                     gofer: gofer.
	
	goferLoad := MetacelloGoferLoad on: gofer.
	goferLoad addResolved: resolvedReference.
	goferLoad execute.
	repo := resolvedReference repository.
	resolvedReference workingCopy repositoryGroup addRepository:
		resolvedReference repository.
	MetacelloNotification signal:
		'Loaded -> ' , resolvedReference name , ' --- '
		, repo repositoryDescription , ' --- '
		, resolvedReference repository description
]

{ #category : 'repositories' }
MetacelloGoferLoader >> repositoriesFrom: aMetacelloMVRepositorySpecs [

	| repositories repos |
	repositories := MCRepositoryGroup default repositories.
	repos := OrderedCollection new.
	aMetacelloMVRepositorySpecs do: [ :aSpec |
		| description |
		description := aSpec description.
		(repositories
			 detect: [ :rep | rep description = description ]
			 ifNone: [ aSpec createRepository ]) ifNotNil: [ :repo |
			repos add: repo ] ].
	^ repos
]

{ #category : 'private' }
MetacelloGoferLoader >> resolvePackageSpecReferences: packageSpec gofer: gofer [

	| versionReference references localGofer |
	localGofer := gofer.
	packageSpec repositorySpecs notEmpty ifTrue: [
		localGofer := Gofer new.
		(self repositoriesFrom: packageSpec repositorySpecs) do: [ :repo |
			localGofer repository: repo ] ].
	packageSpec getFile ifNil: [ "don't use package-cache when trying to get latest version"
		localGofer disablePackageCache ].
	versionReference := packageSpec goferLoaderReference.
	references := versionReference resolveAllWith: localGofer.
	localGofer enablePackageCache.
	^ references
]

{ #category : 'private' }
MetacelloGoferLoader >> resolveRecordingPackageSpecReference: packageSpec gofer: gofer [

	^ (self resolvePackageSpecReferences: packageSpec gofer: gofer) first
]
