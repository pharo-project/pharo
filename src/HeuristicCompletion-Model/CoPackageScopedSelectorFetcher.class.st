"
Provides selector completions from the local package, its direct dependencies, and their dependencies.
"
Class {
	#name : 'CoPackageScopedSelectorFetcher',
	#superclass : 'CoGlobalSelectorFetcher',
	#instVars : [
		'package',
		'completionClass',
		'organizer'
	],
	#classVars : [
		'Dependencies'
	],
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'private' }
CoPackageScopedSelectorFetcher class >> computeDependenciesOf: aPackageName [
	
	^ StDependencyChecker new dependenciesOf: aPackageName
]

{ #category : 'private' }
CoPackageScopedSelectorFetcher class >> dependenciesOf: aPackageName [

	^ self dependencyCache at: aPackageName ifAbsentPut: [ self computeDependenciesOf: aPackageName ]
]

{ #category : 'private' }
CoPackageScopedSelectorFetcher class >> dependencyCache [

	^ Dependencies ifNil: [ Dependencies := Dictionary new ]
]

{ #category : 'private' }
CoPackageScopedSelectorFetcher class >> resetCache [
	<script>
	Dependencies := nil
]

{ #category : 'accessing' }
CoPackageScopedSelectorFetcher >> completionClass [ 

    ^ completionClass
]

{ #category : 'accessing' }
CoPackageScopedSelectorFetcher >> completionClass: aClass [ 

    completionClass := aClass.
    completionClass ifNotNil: [
        package := completionClass package.
        organizer := package organizer ].
]

{ #category : 'accessing' }
CoPackageScopedSelectorFetcher >> dependenciesOf: aString [
	
	^ self class dependenciesOf: aString
]

{ #category : 'accessing' }
CoPackageScopedSelectorFetcher >> packageLocalSelectors: aBlock [ 
    | selectors |
    selectors := OrderedCollection new.
    package definedClasses do: [ :cls | selectors addAll: cls selectors ].
	 selectors addAll: package extensionSelectors.
    selectors do: [ :sel |
        (filter accepts: sel) ifTrue: [
            aBlock value: ((NECGlobalEntry contents: sel node: nil)
                            fetcher: self;
                            yourself) ] ].
    ^ selectors size
]

{ #category : 'accessing' }
CoPackageScopedSelectorFetcher >> treatPackageNamed: aPackageName into: aCollection [ 


			| pkg classes |
					pkg := organizer packageNamed: aPackageName ifAbsent: [ nil ].
					pkg ifNotNil: [
							classes := pkg definedClasses.
							classes do: [ :cls |
								cls selectors do: [ :selector | aCollection add: selector ].
							aCollection addAll: pkg extensionSelectors 	
							] ]
]
