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
			package: 'Tool-CriticBrowser-Tests';
			package: 'Renraku';
			package: 'Renraku-Tests';
			package: 'Renraku-Help'.
		spec 
			group: 'Core' with: #('Renraku' 'Tool-CriticBrowser');
			group: 'Tests' with: #('Renraku-Tests' 'Tool-CriticBrowser-Tests');
			group: 'Help' with: #('Renraku-Help');
			group: 'default' with: #('Core' 'Help' 'Tests') ]
]
