"
I'm a fetcher knowing package.
"
Class {
	#name : 'CoPackageScopedGlobalVariableFetcher',
	#superclass : 'CoGlobalFetcher',
	#instVars : [
		'package',
		'completionClass',
		'organizer'
	],
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'accessing' }
CoPackageScopedGlobalVariableFetcher >> completionClass [

	^ completionClass
]

{ #category : 'accessing' }
CoPackageScopedGlobalVariableFetcher >> completionClass: anObject [

	completionClass := anObject.
	completionClass ifNotNil: [ 
		package := completionClass package.
		organizer := package organizer ]
]

{ #category : 'enumerating' }
CoPackageScopedGlobalVariableFetcher >> entriesDo: aBlock [

	completionClass isNil ifTrue: [ ^ self ].

	self packageLocalClasses: aBlock.
]

{ #category : 'enumerating' }
CoPackageScopedGlobalVariableFetcher >> packageLocalClasses: aBlock [

	| homePackageClassNames |
	homePackageClassNames := SortedCollection sortUsing: [ :a :b | a <= b ].

	package definedClasses do: [ :cls | homePackageClassNames add: cls name ].

	homePackageClassNames do: [ :closeClassName |
			aBlock value: ((CoGlobalEntry contents: closeClassName node: nil)
					 fetcherName: self class name;
					 yourself) ].

	^ homePackageClassNames
]
