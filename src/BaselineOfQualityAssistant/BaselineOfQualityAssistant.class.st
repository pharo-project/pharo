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
			package: 'Tool-CriticBrowser';
			package: 'Renraku';
			package: 'Renraku-Test';
			package: 'Renraku-Help'.
		spec 
			group: 'Core' with: #('Renraku' 'Tool-CriticBrowser');
			group: 'Tests' with: #('Renraku-Test');
			group: 'Help' with: #('Renraku-Help');
			group: 'default' with: #('Core' 'Help' 'Tests') ]
]
