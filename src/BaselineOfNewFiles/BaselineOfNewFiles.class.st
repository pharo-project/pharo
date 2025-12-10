Class {
	#name : 'BaselineOfNewFiles',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfNewFiles',
	#package : 'BaselineOfNewFiles'
}

{ #category : 'baselines' }
BaselineOfNewFiles >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'NewFiles';
			package: 'NewFiles-Tests'.
		spec 
			group: 'Core' with: #('NewFiles');
			group: 'Tests' with: #('NewFiles-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
