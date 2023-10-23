Class {
	#name : 'BaselineOfOSWindow',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfOSWindow',
	#package : 'BaselineOfOSWindow'
}

{ #category : 'baselines' }
BaselineOfOSWindow >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'OSWindow-Core';
			package: 'OSWindow-SDL2';
			package: 'OSWindow-SDL2-Examples';
			package: 'OSWindow-Tests'.
		spec 
			group: 'Core' with: #('OSWindow-Core' 'OSWindow-SDL2' 'OSWindow-SDL2-Examples');
			group: 'Tests' with: #('OSWindow-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
