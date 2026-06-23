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
	super tearDown.
]

{ #category : 'running' }
CoResultSetAdditionalTest >> testExactAndFuzzyMatchesOrder [
	"Exact Matches should Appear before Fuzzy Matches"

	| candidates |
	CoToleranceFilter rate: 0.3.
	"Token length is the minimum between the candidate's length and the token's length
	For each candidate we have : 
		min (SpPresenter, SpPresenter) = 11, max edits = (0.3 * 11 ) floor = 3
		We can have up to 3 edits
	"
	
	completion filter: (CoBeginsWithFilter caseSensitive: true filterString: 'SpPresenter').
	
	
	candidates := (completion fetch: 20) results. 
	
	"We should have : 
		* SpPresenter, an exact match
		* SpPresenterBuilder, another exact match
		* StPresenter, 2 substitutions
		
	 We should not have
		* SpMenuPresenter same as above, needs 4 and we can only do 3
		* Widget, Prsen,  OpalCompiler and SpAbstractWidgetPresenter we need too much edits 
		
	
	 Exact matches : SpPresenter and SpPresenterBuilder must appear before StPresenter
	"
	
	self assert: candidates size equals: 3.
	self assert: (candidates collect: [:e | e contents]) equals: #('SpPresenter' 'SpPresenterBuilder' 'StPresenter') asOrderedCollection.
]

{ #category : 'running' }
CoResultSetAdditionalTest >> testFuzzyMatchesOrder [
	"Exact Matches should Appear before Fuzzy Matches"

	| candidates |
	CoToleranceFilter rate: 0.8 .
	"Token length is the minimum between the candidate's length and the token's length
	For each candidate we have for example : 
		min (SpPreseny, SpPresenter) = 9, max edits = (0.8 * 9 ) floor = 5
		We can have up to 5 edits
	"
	
	"We work only on fuzzy matches"
	completion filter: (CoBeginsWithFilter caseSensitive: true filterString: 'SpPreseny').
	
	
	candidates := (completion fetch: 20) results. 
	
	"We should have : 
		* SpPresenter, SpPresenterBuilder, Only 1 substitution
		* StPresenter, 2 substitutions	
		* SpMenuPresenter and SpAbstractWidgetPresenter distance is 7
		* Prsen, 3 insertions
		
	 We should not have
		* Widget,  OpalCompiler we need too much edits 
	"
	
	self assert: candidates size equals: 6.
	"We test the order"
	self assert: (candidates collect: [:e | e contents]) equals: #('SpPresenter' 'SpPresenterBuilder' 'StPresenter' 'Prsen' SpAbstractWidgetPresenter 'SpMenuPresenter') asOrderedCollection.
]
