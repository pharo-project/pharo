Class {
	#name : 'MetacelloMonticelloLoader',
	#superclass : 'Object',
	#instVars : [
		'projectCache'
	],
	#category : 'Metacello-Core-Gofer',
	#package : 'Metacello-Core',
	#tag : 'Gofer'
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
MetacelloMonticelloLoader >> loadAtomicPackageDirectives: packageDirectives [
	"Each package directive can specify many repositorySpecs.
	Find the first one that defines the package (assumming they all are equivalent) and use that."

	| model references |
	model := MCVersionLoader new.

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
	

	packageDirectives do: [ :e |
		MetacelloNotification signal:
			'Loaded -> ' , e packageName , ' --- '
			, e repositorySpecs first description ]
]

{ #category : 'loading' }
MetacelloMonticelloLoader >> loadPackageDirective: aMetacelloPackageLoadDirective [

	self loadAtomicPackageDirectives: { aMetacelloPackageLoadDirective }
]

{ #category : 'loading' }
MetacelloMonticelloLoader >> loadProject: aMetacelloMCBaselineOfProjectSpec [
	"Loads the project class (BaselineOf, ConfigurationOf) and creates a project from it"

	"There is no need at loading a project twice as long as it has the same version."

	| projectClass projectClassInstance project |
	projectCache
		at: aMetacelloMCBaselineOfProjectSpec className
		ifPresent: [ :found | ^ found ].

	(self lookupProjectClassNamed:
		 aMetacelloMCBaselineOfProjectSpec className) ifNil: [
		self loadPackageDirective: (MetacelloDirective
				 loadPackage: (MetacelloPackageSpec new
						  name: aMetacelloMCBaselineOfProjectSpec className;
						  repositories: aMetacelloMCBaselineOfProjectSpec repositories;
						  yourself)
				 repositorySpecs:
				 aMetacelloMCBaselineOfProjectSpec repositorySpecs) ].

	projectClass := self lookupProjectClassNamed:
		                aMetacelloMCBaselineOfProjectSpec className asSymbol.
	projectClassInstance := projectClass new.
	"Subclasses typically change the load type of the project to #atomic"
	projectClassInstance initalizeProjectWithRepositoryDescription:
		aMetacelloMCBaselineOfProjectSpec repositoryDescriptions.
	project := projectClassInstance project.
	projectCache
		at: aMetacelloMCBaselineOfProjectSpec className
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
	repository := self repositoriesFrom: aCollection.
	aCollection first resolvedReference:
		repository repositoryDescription , '('
		, repository repositoryVersionString , ')'.
	^ aCollection
]
