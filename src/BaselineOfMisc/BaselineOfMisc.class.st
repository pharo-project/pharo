Class {
	#name : 'BaselineOfMisc',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfMisc',
	#package : 'BaselineOfMisc'
}

{ #category : 'baselines' }
BaselineOfMisc >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'PharoDocComment';
			package: 'STON-Text support';
			package: 'System-CommandLine-TextSupport';
			package: 'PerformMessageCommandLineHandler';
			package: 'Files-Prompt'. ]
]
