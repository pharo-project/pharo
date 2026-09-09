Class {
	#name : 'MetacelloMonticelloLoader',
	#superclass : 'Object',
	#instVars : [
		'projectCache'
	],
	#category : 'Metacello-Core-Monticello',
	#package : 'Metacello-Core',
	#tag : 'Monticello'
}

{ #category : 'accessing' }
MetacelloMonticelloLoader >> engine: aMetacelloScriptEngine [ 
	"Nothing"
]

{ #category : 'loading' }
MetacelloMonticelloLoader >> fetchPackageDirective: aMetacelloPackageLoadDirective [

	| repository allResolvedReferences |
	repository := self repositoriesFrom:
		              aMetacelloPackageLoadDirective repositorySpecs.
	allResolvedReferences := repository fetchPackageNamed:
		                         aMetacelloPackageLoadDirective spec name
]

{ #category : 'initialization' }
MetacelloMonticelloLoader >> initialize [

	super initialize.
	projectCache := Dictionary new
]

{ #category : 'loading' }
MetacelloMonticelloLoader >> loadAtomicPackageDirectives: packageDirectives engine: theEngine [
	"Each package directive can specify many repositorySpecs.
	Find the first one that defines the package (assumming they all are equivalent) and use that."

	| model references strictnesses strictness startTime |
	
	startTime := Time millisecondClockValue.
	model := MCVersionLoader new.

	strictnesses := packageDirectives collect: [ :e | e shouldLoadStrict ] as: Set.
	self assert: strictnesses size = 1 description: 'Cannot load atomically packages with different strictness policies'.
	strictness := strictnesses anyOne.
	"If not specified explicitly, use the default one defined in the engine"
	model isStrict: (strictness ifNil: [ theEngine strict ]).

	references := packageDirectives collect: [ :e |
		| repository |
		repository := self repositoriesFrom: e repositorySpecs.
		repository loadPackageNamed: e spec name intoLoader: model.
	].

	model load.
	
	"Add the repositories to monticello. Otherwise this will break during the loading of Pharo.
	See: BaselineOf >> #packageRepository.
	Still used by WelcomeBrowser.
	When this is removed, we should also remove the reference return in loadPackageNamed:intoLoader:"
	references do: [ :e |
		e first workingCopy repositoryGroup addRepository: e second ].
	

	packageDirectives size = 1 
		ifTrue: [ | singlePackage |
			singlePackage := packageDirectives anyOne.
			MetacelloNotification 
				signal: 'Loaded -> ' , singlePackage packageName , ' --- '
					, singlePackage repositorySpecs first fullDescription 
				milliSeconds: (Time millisecondClockValue - startTime)
			
		] ifFalse: [  
			packageDirectives do: [ :e |
				MetacelloNotification signal:
					'Loaded -> ' , e packageName , ' --- '
					, e repositorySpecs first fullDescription ].
				
			MetacelloNotification 
				signal: 'Atomic Loading finished' 
				milliSeconds: (Time millisecondClockValue - startTime)].
]

{ #category : 'loading' }
MetacelloMonticelloLoader >> loadProject: aMetacelloProjectSpec engine: anEngine [
	"Loads the project BaselineOf and creates a project from it 
	 There is no need at loading a project twice as long as it has the same version.
	 But if ignoreImage is true, then we want to load it anyway."
	| projectClass projectClassInstance project |
	
	anEngine ignoreImage ifFalse: [ 
		projectCache 
			at: aMetacelloProjectSpec className 
			ifPresent: [ :found | ^ found ] ].

	(anEngine ignoreImage 
		or: [ (self lookupProjectClassNamed: aMetacelloProjectSpec className) isNil ]) 
		ifTrue: [
			self
				loadAtomicPackageDirectives: { (MetacelloPackageLoadDirective spec: aMetacelloProjectSpec projectPackage) }
				engine: anEngine ].

	projectClass := self lookupProjectClassNamed: aMetacelloProjectSpec className asSymbol.
	
	projectClassInstance := projectClass new. 
	"Subclasses typically change the load type of the project to #atomic"
	projectClassInstance initalizeProjectWithRepositoryDescription: aMetacelloProjectSpec repositoryDescriptions.
	project := projectClassInstance project.
	
	projectCache 
		at: aMetacelloProjectSpec className 
		put: project.
	^ project
]

{ #category : 'accessing' }
MetacelloMonticelloLoader >> lookupProjectClassNamed: aString [

	^ Smalltalk at: aString asSymbol ifAbsent: [ nil ]
]

{ #category : 'repository-resolution' }
MetacelloMonticelloLoader >> repositoriesFrom: aMetacelloMVRepositorySpecs [

	aMetacelloMVRepositorySpecs size = 1 ifTrue: [
		^ aMetacelloMVRepositorySpecs first createRepository ].

	^ MetacelloRepositoryGroup onRepositories:
		  (aMetacelloMVRepositorySpecs collect: [ :aSpec |
			   aSpec createRepository ])
]

{ #category : 'repository-resolution' }
MetacelloMonticelloLoader >> resolveRepositorySpecs: aCollection [
	"Return a resolved repository spec with a concrete commit id or tag"

	| repository |
	aCollection ifEmpty: [ ^ aCollection ].
	repository := self repositoriesFrom: aCollection.
	aCollection first resolvedReference:
		repository repositoryDescription , '('
		, repository repositoryVersionString , ')'.
	^ aCollection
]
