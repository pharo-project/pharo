Class {
	#name : 'CoToleranceFilterTest',
	#superclass : 'TestCase',
	#instVars : [
		'filter',
		'rate'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
CoToleranceFilterTest >> setUp [

	super setUp.
	rate := CoToleranceFilter rate. 
	CoToleranceFilter enable.
	
	filter := CoToleranceFilter new decoree: CoCaseInsensitiveBeginsWithFilter new.
	
	
]

{ #category : 'running' }
CoToleranceFilterTest >> tearDown [  
	CoToleranceFilter rate: rate .
	super tearDown 
]

{ #category : 'running' }
CoToleranceFilterTest >> testComplementResults [
	| candidates results|

	candidates := OrderedCollection new. 
	candidates add: 'AuthenticationService'; add: 'AuthentificationManager';
    					add: 'AuthService'; add: 'AuthorizationService';
					
    					add: 'ConfigurationLoader'; add: 'ConfigurationManager';
					
						add: 'DependencyInjector'.
						
	filter decoree completionString: 'Authe'.
	
	"the token ""Authe"" 
		matches exactly : AuthenticationService and  AuthentificationManager
	 	matches partially AuthService, AuthorizationService
		does not match DependencyInjector, ConfigurationLoader and ConfigurationManager
	"
	
	results := filter complementResults: candidates.
	self assert: results first equals: 'AuthenticationService'. 
	self assert: results second equals: 'AuthentificationManager'. 
	self assert: (results includes: 'AuthService').
	{'DependencyInjector' . 'ConfigurationLoader'}
		do: [ :e | self deny: (results includes: e)]
	 
]

{ #category : 'running' }
CoToleranceFilterTest >> testFuzzyAcceptsWhenAlmostExactMatch [
	filter decoree completionString: 'SpPresnter'.
	self assert: (filter fuzzyAccepts: 'SpPresenter')
]

{ #category : 'running' }
CoToleranceFilterTest >> testFuzzyAcceptsWhenNoMatch [
	filter decoree completionString: 'core'.
	self deny: (filter fuzzyAccepts: 'database')
]

{ #category : 'running' }
CoToleranceFilterTest >> testFuzzyScoreForWhenBigDifference [
	filter decoree completionString: 'SpAbs'.
	self assert: (filter fuzzyScoreFor: 'SpAbstractWidgetPresenter') equals: 0
]

{ #category : 'running' }
CoToleranceFilterTest >> testFuzzyScoreForWhenCandidateIsEmpty [
	filter decoree completionString: 'core'.
	self assert: (filter fuzzyScoreFor: '') equals: 0
]

{ #category : 'running' }
CoToleranceFilterTest >> testFuzzyScoreForWhenCandidateStartExactlyWithToken [
	"When there is an exact match, the fuzzy score is null."
	
	filter decoree completionString: 'Database'.
	self assert: (filter fuzzyScoreFor: 'DatabaseConnection') equals: 0
]

{ #category : 'running' }
CoToleranceFilterTest >> testFuzzyScoreForWhenKeywordArityIsChanged [

	filter decoree completionString: 'message:param1:'.
	self assert: (filter fuzzyScoreFor: 'messageparam1') equals: 0
]

{ #category : 'running' }
CoToleranceFilterTest >> testFuzzyScoreForWhenTokenAndCandidateAreDifferent [
	filter decoree completionString: 'core'.
	self assert: (filter fuzzyScoreFor: 'database') equals: 0
]

{ #category : 'running' }
CoToleranceFilterTest >> testFuzzyScoreForWhenTokenContainsTypo [
	filter decoree completionString: 'Datebase'.
	self assert: (filter fuzzyScoreFor: 'DatabaseConnection') > CoToleranceFilter rate
]

{ #category : 'running' }
CoToleranceFilterTest >> testFuzzyScoreForWhenTokenIsEmpty [
	filter decoree completionString: ''.
	self assert: (filter fuzzyScoreFor: 'database') equals: 0
]

{ #category : 'running' }
CoToleranceFilterTest >> testFuzzyScoreForWhenTokenSizeIsLessThanMinimusTokenSize [
	CoToleranceFilter disable.
	filter decoree completionString: 'D'.
	self assert: (filter fuzzyScoreFor: 'DatabaseConnection') equals: 0
]

{ #category : 'running' }
CoToleranceFilterTest >> testFuzzyScoreForWhenToleranceIsDisabled [
	CoToleranceFilter disable.
	filter decoree completionString: 'Databas'.
	self assert: (filter fuzzyScoreFor: 'DatabaseConnection') equals: 0
]

{ #category : 'running' }
CoToleranceFilterTest >> testRequiresFallbackEnumerationWhenDisabled [

	CoToleranceFilter disable.
	filter decoree completionString: 'Datbase'.
	self deny: filter requiresFallbackEnumeration 
]

{ #category : 'running' }
CoToleranceFilterTest >> testRequiresFallbackEnumerationWhenTokenSizeIsLessThanMinimumTokenSize [

	CoToleranceFilter enable.
	self assert: CoToleranceFilter rate > 0.
	filter decoree completionString: 'D'.
	self deny: filter requiresFallbackEnumeration 
]
