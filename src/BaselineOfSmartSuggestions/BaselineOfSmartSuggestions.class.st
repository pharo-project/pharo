Class {
	#name : #BaselineOfSmartSuggestions,
	#superclass : #BaselineOf,
	#category : #BaselineOfSmartSuggestions
}

{ #category : #baselines }
BaselineOfSmartSuggestions >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'SmartSuggestions';
			package: 'SmartSuggestions-Tests'.
		spec 
			group: 'Core' with: #('SmartSuggestions');
			group: 'Tests' with: #('SmartSuggestions-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
