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
MetacelloLoadTarget >> executeOnSpec: aSpec withEngine: anEngine [

	self linearLoadPackagesOfSpec: aSpec withEngine: anEngine
]

{ #category : 'private' }
MetacelloLoadTarget >> linearLoadPackageSpec: packageSpec gofer: gofer withEngine: anEngine [

	| goferLoad resolvedReference repo |

	resolvedReference := self
		                     resolvePackageSpec: packageSpec
		                     gofer: gofer.
	resolvedReference ifNil: [ "Package version already loaded into image"
		^ self ].

	self preLoad: packageSpec withEngine: anEngine.
	goferLoad := MetacelloGoferLoad on: Gofer new.
	goferLoad addResolved: resolvedReference.
	goferLoad execute.
	self postLoad: packageSpec withEngine: anEngine.

	repo := resolvedReference repository.
	resolvedReference workingCopy repositoryGroup addRepository:
		resolvedReference repository.
	MetacelloNotification signal:
		'Loaded -> ' , resolvedReference name , ' --- '
		, repo repositoryDescription , ' --- '
		, resolvedReference repository description
]

{ #category : 'actions' }
MetacelloLoadTarget >> loadPackageDirectives: pkgLoads gofer: aGofer [

	| goferLoad |
	goferLoad := MetacelloGoferLoad on: aGofer.

	pkgLoads do: [ :packageLoadDirective |
		| packageSpec |
		packageSpec := packageLoadDirective spec.
		aGofer disablePackageCache. "for good luck:)"
		(self resolvePackageSpec: packageSpec gofer: aGofer) ifNotNil: [
			:resolvedReference |
			goferLoad addResolved: resolvedReference.
			packageLoadDirective resolvedReference: resolvedReference ] ].


	goferLoad execute.
	pkgLoads do: [ :packageLoadDirective |
		packageLoadDirective resolvedReference
			ifNotNil: [
				MetacelloNotification
					signal: 'Loaded -> ' , packageLoadDirective file , ' --- '
						, packageLoadDirective repository repositoryDescription
						, ' --- '
						, packageLoadDirective resolvedReference repository description
					level: 2.
				packageLoadDirective resolvedReference workingCopy repositoryGroup
					addRepository: packageLoadDirective repository ]
			ifNil: [
				MetacelloNotification
					signal: 'Already Loaded -> ' , packageLoadDirective file
					level: 2 ] ]
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

{ #category : 'private' }
MetacelloLoadTarget >> resolvePackageSpec: packageSpec gofer: gofer [

	| references resolvedReference mcVersion |
	references := self
		              retryingResolvePackageSpecReferences: packageSpec
		              gofer: gofer.
	resolvedReference := references last
		                     asMetacelloCachingResolvedReference.
	mcVersion := resolvedReference version.
	packageSpec ancestors ifNotNil: [ :loadedVersionInfos |
		loadedVersionInfos do: [ :info |
			info name = mcVersion info name ifTrue: [
				| spc |
				"package already loaded, don't load again"
				spc := packageSpec copy.
				spc file: info name.
				(MetacelloIgnorePackageLoaded signal: spc) ifFalse: [ ^ nil ] ] ] ].
	^ resolvedReference
]