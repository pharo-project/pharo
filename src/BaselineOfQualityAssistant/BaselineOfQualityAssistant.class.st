Class {
	#name : 'BaselineOfQualityAssistant',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfQualityAssistant',
	#package : 'BaselineOfQualityAssistant'
}

{ #category : 'baselines' }
BaselineOfQualityAssistant >> baseline: spec [
	<baseline>
	
	spec for: #'common' do: [
			
		spec 
			package: 'Renraku';
			package: 'Renraku-Tests'.
		spec 
			group: 'Core' with: #('Renraku');
			group: 'Tests' with: #('Renraku-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
