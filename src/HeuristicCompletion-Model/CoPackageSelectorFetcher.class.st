"
I'm the root of the fetcher supporting message level (selector) package scope lookup. For example, my subclass `CoDependencySelector` will look first in all the selectors of the classes of the current package (the package of the class in which the completion request is performed) then it will look in the selector of its dependent packages.

#### Implementation note. 
Notice that computing the dependency all the times may take time. During our intensive benchmarks this got a real impact on the overall times. Therefore I'm caching the results at the class level. Since the cache is quite naive it is a simple dictionary and we did not work on its invalidation, it may happen that the completion will report off suggestions when the dependency structure changed under its feet. A future work is to improve this either by removing the cache and measuring if it is really needed during real human driven sessions or invalidating it when a class is removed from a package.
"
Class {
	#name : 'CoPackageSelectorFetcher',
	#superclass : 'CoGlobalSelectorFetcher',
	#instVars : [
		'package',
		'organizer',
		'completionClass'
	],
	#classVars : [
		'Dependencies'
	],
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'accessing' }
CoPackageSelectorFetcher class >> computeDependenciesOf: aPackageName [

	^ StDependencyChecker new dependenciesOf: aPackageName
]

{ #category : 'accessing' }
CoPackageSelectorFetcher class >> dependenciesOf: aPackageName [

	^ self dependencyCache at: aPackageName ifAbsentPut: [ self computeDependenciesOf: aPackageName ]
]

{ #category : 'accessing' }
CoPackageSelectorFetcher class >> dependencyCache [

	^ Dependencies ifNil: [ Dependencies := Dictionary new ]
]

{ #category : 'accessing' }
CoPackageSelectorFetcher class >> resetCache [

	<script>
	Dependencies := nil
]

{ #category : 'accessing' }
CoPackageSelectorFetcher >> completionClass [

	^ completionClass
]

{ #category : 'accessing' }
CoPackageSelectorFetcher >> completionClass: aClass [

	completionClass := aClass.
	completionClass ifNotNil: [
			package := completionClass package.
			organizer := package organizer ]
]

{ #category : 'accessing' }
CoPackageSelectorFetcher >> dependenciesOf: aString [

	^ self class dependenciesOf: aString
]

{ #category : 'accessing' }
CoPackageSelectorFetcher >> packageLocalSelectors: aBlock [

	| selectors |
	selectors := OrderedCollection new.
	package definedClasses do: [ :cls | selectors addAll: cls selectors ].
	selectors addAll: package extensionSelectors.
	selectors do: [ :sel |
			aBlock value: ((CoSelectorEntry contents: sel node: astNode)
					 fetcherName: self class name;
					 yourself) ].
	^ selectors size
]

{ #category : 'accessing' }
CoPackageSelectorFetcher >> treatPackageNamed: aPackageName into: aCollection [

	| pkg classes |
	pkg := organizer packageNamed: aPackageName ifAbsent: [ nil ].
	pkg ifNotNil: [
			classes := pkg definedClasses.
			classes do: [ :cls |
					cls selectors do: [ :selector | aCollection add: selector ].
					aCollection addAll: pkg extensionSelectors ] ]
]
