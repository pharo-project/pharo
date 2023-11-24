Class {
	#name : 'MetacelloLoadTarget',
	#superclass : 'MetacelloTarget',
	#category : 'Metacello-Core-Loaders',
	#package : 'Metacello-Core',
	#tag : 'Loaders'
}

{ #category : 'accessing' }
MetacelloLoadTarget >> actionLabel [

	^'Loading '
]

{ #category : 'accessing' }
MetacelloLoadTarget >> asFetchingTarget [

	^(MetacelloFetchTarget on: self spec)
		"loaderPolicy: self loaderPolicy copy;"
		yourself
]

{ #category : 'executing' }
MetacelloLoadTarget >> executeOnSpec: aSpec [

	self linearLoadPackagesOfSpec: aSpec
]

{ #category : 'executing' }
MetacelloLoadTarget >> executeOnSpec: aSpec withEngine: anEngine [

	self linearLoadPackagesOfSpec: aSpec withEngine: anEngine
]

{ #category : 'private' }
MetacelloLoadTarget >> linearLoadPackageSpec: packageSpec gofer: gofer withEngine: anEngine [

	MetacelloPlatform current
		do: [
			| loadBlock goferLoad answers resolvedReference repo |
			resolvedReference := self
				                     resolvePackageSpec: packageSpec
				                     gofer: gofer.
			resolvedReference ifNil: [ "Package version already loaded into image"
				^ self ].
			loadBlock := [
			             self preLoad: packageSpec withEngine: anEngine.
			             goferLoad := MetacelloGoferLoad on: Gofer new.
			             goferLoad addResolved: resolvedReference.
			             goferLoad execute.
			             self postLoad: packageSpec withEngine: anEngine ].
			(answers := packageSpec answers) notEmpty
				ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
				ifFalse: [ loadBlock value ].
			repo := resolvedReference repository.
			"self hasRepositoryOverrides
				ifTrue: [
					repo := self loaderPolicy repositoryMap
						        at: resolvedReference name
						        ifAbsent: [ resolvedReference repository ].
					resolvedReference workingCopy repositoryGroup addRepository: repo ]
				ifFalse: ["
			resolvedReference workingCopy repositoryGroup addRepository:
				resolvedReference repository. "]"
			MetacelloNotification signal:
				'Loaded -> ' , resolvedReference name , ' --- '
				, repo repositoryDescription , ' --- '
				, resolvedReference repository description ]
		displaying: 'Loading ' , packageSpec file
]

{ #category : 'doits' }
MetacelloLoadTarget >> postLoad: packageOrVersionSpec withEngine: anEngine [

	packageOrVersionSpec postLoadDoItBlock ifNotNil: [ :block |
		block valueWithPossibleArgs: {
				self.
				packageOrVersionSpec } ]
]

{ #category : 'doits' }
MetacelloLoadTarget >> preLoad: packageOrVersionSpec withEngine: anEngine [

	packageOrVersionSpec preLoadDoItBlock ifNotNil: [ :block |
		block valueWithPossibleArgs: {
				self.
				packageOrVersionSpec } ]
]
