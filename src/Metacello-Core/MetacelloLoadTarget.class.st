Class {
	#name : 'MetacelloLoadTarget',
	#superclass : 'MetacelloTarget',
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'accessing' }
MetacelloLoadTarget >> actionLabel [

	^'Loading '
]

{ #category : 'private' }
MetacelloLoadTarget >> linearLoadPackageSpec: packageSpec gofer: gofer [

	| goferLoad resolvedReference repo |
	resolvedReference := self
		                     resolvePackageSpec: packageSpec
		                     gofer: gofer.
	resolvedReference ifNil: [ "Package version already loaded into image"
		^ self ].

	self preLoad: packageSpec.
	goferLoad := MetacelloGoferLoad on: Gofer new.
	goferLoad addResolved: resolvedReference.
	goferLoad execute.
	self postLoad: packageSpec.

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
MetacelloLoadTarget >> postLoad: packageOrVersionSpec [

	packageOrVersionSpec postLoadDoItBlock ifNotNil: [ :block |
		block valueWithPossibleArgs: {
				self.
				packageOrVersionSpec } ]
]

{ #category : 'doits' }
MetacelloLoadTarget >> preLoad: packageOrVersionSpec [

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

{ #category : 'visiting' }
MetacelloLoadTarget >> visitLinearLoadDirective: aMetacelloLinearLoadDirective [

	aMetacelloLinearLoadDirective loadDirectives do: [ :dir |
		dir acceptVisitor: self ]
]

{ #category : 'visiting' }
MetacelloLoadTarget >> visitPackageLoadDirective: aMetacelloPackageLoadDirective [ 
	
	| goferLoad resolvedReference repo |
	resolvedReference := aMetacelloPackageLoadDirective externalReference.
	resolvedReference ifNil: [ "Package version already loaded into image"
		^ self ].

	self preLoad: aMetacelloPackageLoadDirective spec.
	goferLoad := MetacelloGoferLoad on: Gofer new.
	goferLoad addResolved: resolvedReference.
	goferLoad execute.
	self postLoad: aMetacelloPackageLoadDirective spec.

	repo := resolvedReference repository.
	resolvedReference workingCopy repositoryGroup addRepository:
		resolvedReference repository.
	MetacelloNotification signal:
		'Loaded -> ' , resolvedReference name , ' --- '
		, repo repositoryDescription , ' --- '
		, resolvedReference repository description
]
