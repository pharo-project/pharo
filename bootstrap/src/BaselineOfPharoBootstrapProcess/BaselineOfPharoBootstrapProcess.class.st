"
I'm the configuration used to load everything needed to perform the bootstrap proces:
- ConfigurationOfPharoBootstrap: the definition of the Pharo packages to bootstrap
- Espell : is basic to run the bootstrap
- Ficus:  is the meta model we use to load the code to bootstrap
- Cargo: the package manager that will give us Monticello definitions for Ficus
"
Class {
	#name : #BaselineOfPharoBootstrapProcess,
	#superclass : #BaselineOf,
	#category : 'BaselineOfPharoBootstrapProcess'
}

{ #category : #baselines }
BaselineOfPharoBootstrapProcess >> baseline: spec [
	<baseline>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec baseline: 'Tonel' with: [ spec
			repository: 'github://pharo-vcs/tonel:v1.0.13' ].
		spec baseline: 'Hermes' with: [ spec
			repository: 'github://tesonep/hermes:v2.5.1';
  			loads: 'core' ].
		spec
			 package: 'Pharo30Bootstrap'.
		spec baseline: 'Ring2' with: [ spec
			repository: 'github://carolahp/Ring2:v1.2.2/src';
  			loads: 'bootstrap' ].  			
]
]
