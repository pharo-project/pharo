Class {
	#name : 'CoResultSetAdditionalTest',
	#superclass : 'CoBasicCompletionTest',
	#instVars : [
		'completion',
		'rate'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoResultSetAdditionalTest >> setUp [

	super setUp.
	rate := CoToleranceFilter rate. 
	CoToleranceFilter enable.
    systemNavigation := CoMockSystemNavigation new
        globals: {
				'StPresenter'.
	
            'SpPresenter'.
            'SpAbstractWidgetPresenter'.
				'SpMenuPresenter'.
				'SpPresenterBuilder'.

				'Prsen'.
            'Widget'.
            'OpalCompiler' };
        yourself.
    completion := CoResultSet
		fetcher: (CoGlobalVariableFetcher new
			completionClass: self completionClass; 
			systemNavigation: systemNavigation 
			yourself).
]

{ #category : 'running' }
CoResultSetAdditionalTest >> tearDown [ 
	CoToleranceFilter rate: rate.
	CoToleranceFilter disable. 
	super tearDown.
]

{ #category : 'running' }
CoResultSetAdditionalTest >> testExactAndFuzzyMatchesOrder [
	"Exact Matches should Appear before Fuzzy Matches"

	| candidates |
	CoToleranceFilter rate: 0.3.
	
	completion filter: (CoBeginsWithFilter caseSensitive: true filterString: 'SpPresenter').
	
	
	candidates := (completion fetch: 20) results. 

	self assert: candidates size equals: 2.
	self assert: (candidates collect: [:e | e contents]) equals: #('SpPresenter' 'SpPresenterBuilder') asOrderedCollection.
]

{ #category : 'running' }
CoResultSetAdditionalTest >> testFuzzyMatchesOrder [
	"Exact Matches should Appear before Fuzzy Matches"

	| candidates |
	CoToleranceFilter rate: 0.8.
	CoToleranceFilter enable.
	
	"We work only on fuzzy matches"
	completion filter: (CoBeginsWithFilter caseSensitive: true filterString: 'SpPreseny').
	
	candidates := (completion fetch: 20) results. 

	
	self assert: candidates size equals: 2.
	"We test the order"
	self assert: (candidates collect: [:e | e contents]) equals: #('SpPresenter' 'SpPresenterBuilder') asOrderedCollection.
]
