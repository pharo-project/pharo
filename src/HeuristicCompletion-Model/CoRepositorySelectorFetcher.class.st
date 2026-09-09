"
Look in the package and the other packages starting with the same prefix name e.g., HeuristicCompletion-Analyser.... for HeuristicCompletion-Model.
"
Class {
	#name : 'CoRepositorySelectorFetcher',
	#superclass : 'CoPackageSelectorFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoRepositorySelectorFetcher >> entriesDo: aBlock [

	| packageNamePart friendPackageNames friendSelectors |
	completionClass isNil ifTrue: [ ^ self ].

	self packageLocalSelectors: aBlock. "Extract base prefix of the current package"

	packageNamePart := package name copyUpTo: $-.
	friendPackageNames := organizer packageNames asOrderedCollection.
	friendPackageNames remove: package name ifAbsent: [ ].

	friendSelectors := Set new.
	friendPackageNames do: [ :eachPackageName |
		(packageNamePart , '*' match: eachPackageName) ifTrue: [ self treatPackageNamed: eachPackageName into: friendSelectors ] ].

	friendSelectors do: [ :sel |
			
					aBlock value: ((CoSelectorEntry contents: sel node: astNode)
							 fetcherName: self class name;
							 yourself) ] 
]
