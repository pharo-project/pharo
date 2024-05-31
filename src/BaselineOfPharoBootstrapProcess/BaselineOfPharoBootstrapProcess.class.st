"
I'm the configuration used to load everything needed to perform the bootstrap process
"
Class {
	#name : 'BaselineOfPharoBootstrapProcess',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfPharoBootstrapProcess',
	#package : 'BaselineOfPharoBootstrapProcess'
}

{ #category : 'baselines' }
BaselineOfPharoBootstrapProcess >> baseline: spec [
	<baseline>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec baseline: 'Hermes' with: [ spec
			repository: 'github://pharo-project/hermes:v3.0.1';
  			loads: 'core' ].
		spec
			 package: 'PharoBootstrap' ]
]
