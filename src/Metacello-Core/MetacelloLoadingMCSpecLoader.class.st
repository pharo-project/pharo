Class {
	#name : 'MetacelloLoadingMCSpecLoader',
	#superclass : 'MetacelloSpecLoader',
	#category : 'Metacello-Core-Loaders',
	#package : 'Metacello-Core',
	#tag : 'Loaders'
}

{ #category : 'accessing' }
MetacelloLoadingMCSpecLoader >> actionLabel [

	^'Loading '
]

{ #category : 'accessing' }
MetacelloLoadingMCSpecLoader >> fetchingSpecLoader [

	^(MetacelloFetchingMCSpecLoader on: self spec)
		loaderPolicy: self loaderPolicy copy;
		yourself
]

{ #category : 'private' }
MetacelloLoadingMCSpecLoader >> linearLoadPackageSpec: packageSpec gofer: gofer [

	MetacelloPlatform current
		do: [
			| loadBlock goferLoad answers resolvedReference repo |
			resolvedReference := self resolvePackageSpec: packageSpec gofer: gofer.
			resolvedReference ifNil: [ "Package version already loaded into image" ^ self ].
			loadBlock := [
			             self preLoad: packageSpec.
			             goferLoad := MetacelloGoferLoad on: MetacelloGofer new.
			             goferLoad addResolved: resolvedReference.
			             goferLoad execute.
			             self clearCurrentVersionCache.
			             self postLoad: packageSpec ].
			(answers := packageSpec answers) notEmpty
				ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
				ifFalse: [ loadBlock value ].
			repo := resolvedReference repository.
			self hasRepositoryOverrides
				ifTrue: [
					repo := self loaderPolicy repositoryMap at: resolvedReference name ifAbsent: [ resolvedReference repository ].
					resolvedReference workingCopy repositoryGroup addRepository: repo ]
				ifFalse: [ resolvedReference workingCopy repositoryGroup addRepository: resolvedReference repository ].
			MetacelloNotification signal:
				'Loaded -> ' , resolvedReference name , ' --- ' , repo repositoryDescription , ' --- ' , resolvedReference repository description ]
		displaying: 'Loading ' , packageSpec file
]

{ #category : 'doits' }
MetacelloLoadingMCSpecLoader >> postLoad: packageOrVersionSpec [

	packageOrVersionSpec postLoadDoItBlock ifNotNil: [ :block |
		block valueWithPossibleArgs: {
				self.
				packageOrVersionSpec } ]
]

{ #category : 'doits' }
MetacelloLoadingMCSpecLoader >> preLoad: packageOrVersionSpec [

	packageOrVersionSpec preLoadDoItBlock ifNotNil: [ :block |
		block valueWithPossibleArgs: {
				self.
				packageOrVersionSpec } ]
]
