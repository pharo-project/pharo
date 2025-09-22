"
I'm a fetcher supporting group of packages
"
Class {
	#name : 'CoAbstractPackageDependentScopeGlobalVariableFetcher',
	#superclass : 'CoPackageScopedGlobalVariableFetcher',
	#classVars : [
		'Dependencies'
	],
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'accessing' }
CoAbstractPackageDependentScopeGlobalVariableFetcher class >> computeDependenciesOf: aPackageName [
	
	^ StDependencyChecker new dependenciesOf: aPackageName
]

{ #category : 'accessing' }
CoAbstractPackageDependentScopeGlobalVariableFetcher class >> dependenciesOf: aPackageName [

	^ self dependencyCache at: aPackageName ifAbsentPut: [ self computeDependenciesOf: aPackageName ]
]

{ #category : 'accessing' }
CoAbstractPackageDependentScopeGlobalVariableFetcher class >> dependencyCache [

	^ Dependencies ifNil: [ Dependencies := Dictionary new ]
]

{ #category : 'accessing' }
CoAbstractPackageDependentScopeGlobalVariableFetcher class >> resetCache [
	<script>
	Dependencies := nil
]

{ #category : 'enumerating' }
CoAbstractPackageDependentScopeGlobalVariableFetcher >> dependenciesOf: aString [

	^ self class dependenciesOf: aString
]
