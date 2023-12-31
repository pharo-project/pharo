Class {
	#name : 'MetacelloMonticelloLoader',
	#superclass : 'Object',
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

{ #category : 'loading' }
MetacelloMonticelloLoader >> loadAtomicPackageDirectives: packageDirectives [
	"Each package directive can specify many repositorySpecs.
	Find the first one that defines the package (assumming they all are equivalent) and use that."

	| model |
	model := MCVersionLoader new.

	packageDirectives do: [ :e |
		| repository |
		repository := self repositoriesFrom: e repositorySpecs.
		repository loadPackageNamed: e spec name intoLoader: model ].

	model load.

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

	| projectClass projectClassInstance |
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
	^ projectClassInstance project
]

{ #category : 'accessing' }
MetacelloMonticelloLoader >> lookupProjectClassNamed: aString [

	^ Smalltalk at: aString asSymbol ifAbsent: [ nil ]
]

{ #category : 'repositories' }
MetacelloMonticelloLoader >> repositoriesFrom: aMetacelloMVRepositorySpecs [

	aMetacelloMVRepositorySpecs size = 1 ifTrue: [
		^ aMetacelloMVRepositorySpecs first createRepository ].

	^ MetacelloRepositoryGroup onRepositories:
		  (aMetacelloMVRepositorySpecs collect: [ :aSpec |
			   aSpec createRepository ])
]
