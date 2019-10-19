Class {
	#name : #BaselineOfPharoGT,
	#superclass : #BaselineOf,
	#category : #BaselineOfPharoGT
}

{ #category : #baselines }
BaselineOfPharoGT >> baseline: spec [
	<baseline>
	
	| repository | 
	
	repository := self packageRepositoryURL.	

	spec for: #'common' do: [
		
		spec baseline: 'SmartSuggestions' with: [ spec repository: repository ].
		
		spec 
			package: 'OpalCompiler-Tools';
			package: 'GT-Inspector';
			package: 'GT-InspectorExtensions-Core';
			package: 'GT-Playground';
			package: 'GT-EventRecorder';
			package: 'GT-Spotter';
			package: 'GT-Spotter-UI';
			package: 'GT-SpotterExtensions-Core';
			package: 'GT-Debugger';
			package: 'GT-BytecodeDebugger';
			package: 'GT-SUnitDebugger';
			package: 'GT-Tests-Debugger';
			package: 'GT-Tests-Inspector';
			package: 'GT-Tests-Playground';
			package: 'GT-Tests-Spotter'.
		spec 
			group: 'Core' with: #('OpalCompiler-Tools' 	'GT-Inspector' 	'GT-InspectorExtensions-Core'
				'GT-Playground'	'GT-EventRecorder'	'GT-Spotter'	'GT-Spotter-UI'	'GT-SpotterExtensions-Core'
				'GT-Debugger' 'GT-BytecodeDebugger'	'GT-SUnitDebugger');
			group: 'Tests' with: #('GT-Tests-Debugger' 	'GT-Tests-Inspector' 	'GT-Tests-Playground'
				'GT-Tests-Spotter');
			group: 'default' with: #('Core' 'Tests') ]
]
