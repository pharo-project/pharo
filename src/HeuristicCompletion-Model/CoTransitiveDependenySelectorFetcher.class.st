"
Provides selector completions from the local package, its direct dependencies, and their dependencies.
"
Class {
	#name : 'CoTransitiveDependenySelectorFetcher',
	#superclass : 'CoPackageSelectorFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoTransitiveDependenySelectorFetcher >> entriesDo: aBlock [

	| dependentPackages twoLevelPackages dependentSelectors |
	completionClass isNil ifTrue: [ ^ self ].

	self packageLocalSelectors: aBlock.

	dependentSelectors := Set new.
	dependentPackages := self dependenciesOf: package name.
	twoLevelPackages := dependentPackages asSet.

	dependentPackages do: [ :each | twoLevelPackages addAll: (self dependenciesOf: each) ].

	twoLevelPackages do: [ :pkgName | self treatPackageNamed: pkgName into: dependentSelectors ].

	dependentSelectors do: [ :sel |
		
					aBlock value: ((CoSelectorEntry contents: sel node: astNode)
							 fetcherName: self class name;
							 yourself) ] 
]
