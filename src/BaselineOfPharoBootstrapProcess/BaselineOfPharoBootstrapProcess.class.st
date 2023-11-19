"
I'm the configuration used to load everything needed to perform the bootstrap proces:
- ConfigurationOfPharoBootstrap: the definition of the Pharo packages to bootstrap
- Espell : is basic to run the bootstrap
- Ficus:  is the meta model we use to load the code to bootstrap
- Cargo: the package manager that will give us Monticello definitions for Ficus
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
