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
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [
			spec baseline: 'SUnit' with: [ spec loads: 'Core' from: repository ].
			spec baseline: 'Kernel' with: [ spec loads: 'Tests' from: repository ].
			spec baseline: 'Compiler' with: [ spec loads: 'Tests' from: repository ].
			spec baseline: 'FileSystem' with: [ spec loads: 'TestsFull' from: repository ].
			spec baseline: 'Slot' with: [ spec loads: 'core' from: repository ].
			spec baseline: 'Beacon' with: [ spec loads: #( 'CoreTests' ) from: repository ].
			spec baseline: 'SystemBenchmark' with: [ spec loads: #( 'tests' ) from: repository ].
			spec baseline: 'Clap' with: [ spec loads: #( 'Core' ) from: repository ].
			spec baseline: 'TaskIt' with: [ spec loads: #( 'coreTests' ) from: repository ] ]
]
