Class {
	#name : #BaselineOfFuzzyMatcher,
	#superclass : #BaselineOf,
	#category : #BaselineOfFuzzyMatcher
}

{ #category : #baselines }
BaselineOfFuzzyMatcher >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'FuzzyMatcher';
			package: 'FuzzyMatcher-Tests'.

		spec 
			group: 'Core' with: #('FuzzyMatcher');
			group: 'Tests' with: #('FuzzyMatcher-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
