"
I'm a fetcher supporting group of packages
"
Class {
	#name : 'CoAbstractPackageDependencyFetcher',
	#superclass : 'CoPackageScopedGlobalVariableFetcher',
	#classVars : [
		'Dependencies'
	],
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'accessing' }
CoAbstractPackageDependencyFetcher class >> computeDependenciesOf: aPackageName [
	
	^ StDependencyChecker new dependenciesOf: aPackageName
]

{ #category : 'accessing' }
CoAbstractPackageDependencyFetcher class >> dependenciesOf: aPackageName [

	^ self dependencyCache at: aPackageName ifAbsentPut: [ self computeDependenciesOf: aPackageName ]
]

{ #category : 'accessing' }
CoAbstractPackageDependencyFetcher class >> dependencyCache [

	^ Dependencies ifNil: [ Dependencies := Dictionary new ]
]

{ #category : 'accessing' }
CoAbstractPackageDependencyFetcher class >> resetCache [
	<script>
	Dependencies := nil
]

{ #category : 'enumerating' }
CoAbstractPackageDependencyFetcher >> dependenciesOf: aString [

	^ self class dependenciesOf: aString
]
