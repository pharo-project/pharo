"
Baseline to load libraries used in base image:
 - SUnit (Unit Testing framework)
 - Beacon (Logging framework)
 - Clap (Command line handling)
 - ...
"
Class {
	#name : 'BaselineOfBaseLibraries',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfBaseLibraries',
	#package : 'BaselineOfBaseLibraries'
}

{ #category : 'baselines' }
BaselineOfBaseLibraries >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec repository: (self packageRepositoryURLForSpec: spec).
			spec baseline: 'SUnit' with: [ spec loads: 'Core' ].
			spec baseline: 'Kernel' with: [ spec loads: 'Tests' ].
			spec baseline: 'Compiler' with: [ spec loads: 'Tests' ].
			spec baseline: 'FileSystem' with: [ spec loads: 'Tests' ].
			spec baseline: 'Slot' with: [ spec loads: 'core' ].
			spec baseline: 'Beacon' with: [ spec loads: #( 'CoreTests' ) ].
			spec baseline: 'SystemBenchmark' with: [ spec loads: #( 'tests' ) ].
			spec baseline: 'Clap' with: [ spec loads: #( 'core' ) ].
			spec baseline: 'TaskIt' with: [ spec loads: #( 'coreTests' ) ] ]
]
