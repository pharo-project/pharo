Class {
	#name : #BaselineOfQualityAssistant,
	#superclass : #BaselineOf,
	#category : #BaselineOfQualityAssistant
}

{ #category : #baselines }
BaselineOfQualityAssistant >> baseline: spec [
	<baseline>
	
	spec for: #'common' do: [
			
		spec 
			package: 'Renraku';
			package: 'Renraku-Tests';
			package: 'Renraku-Help'.
		spec 
			group: 'Core' with: #('Renraku');
			group: 'Tests' with: #('Renraku-Tests');
			group: 'Help' with: #('Renraku-Help');
			group: 'default' with: #('Core' 'Help' 'Tests') ]
]
