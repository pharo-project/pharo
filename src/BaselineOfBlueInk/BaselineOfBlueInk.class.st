Class {
	#name : #BaselineOfBlueInk,
	#superclass : #BaselineOf,
	#category : #BaselineOfBlueInk
}

{ #category : #baselines }
BaselineOfBlueInk >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'BlueInk-Core';
			package: 'BlueInk-Extras';
			package: 'BlueInk-Tests'.
		spec 
			group: 'Core' with: #('BlueInk-Core' 'BlueInk-Extras');
			group: 'Extras' with: #('BlueInk-Extras');
			group: 'Tests' with: #('BlueInk-Tests');
			group: 'default' with: #('Core' 'Extras' 'Tests') ]
]
