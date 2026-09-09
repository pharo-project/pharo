"
Suggests selectors visible from the local package and its direct dependencies.
"
Class {
	#name : 'CoDependencySelectorFetcher',
	#superclass : 'CoPackageSelectorFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoDependencySelectorFetcher >> entriesDo: aBlock [

	| dependentPackages dependentSelectors |
	completionClass isNil ifTrue: [ ^ self ].

	self packageLocalSelectors: aBlock.

	dependentSelectors := Set new.
	dependentPackages := self dependenciesOf: package name.

	dependentPackages do: [ :pkgName | self treatPackageNamed: pkgName into: dependentSelectors ].

	dependentSelectors do: [ :sel |
			aBlock value: ((CoSelectorEntry contents: sel node: astNode)
					 fetcherName: self class name;
					 yourself) ]
]
