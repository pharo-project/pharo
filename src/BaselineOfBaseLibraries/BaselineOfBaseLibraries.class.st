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
		spec baseline: 'SUnitCore' with: [
			spec
				loads: 'Core';
				className: 'BaselineOfSUnit';
				repository: repository ].

		spec baseline: 'KernelTests' with: [ spec repository: repository ].

		spec baseline: 'Slot' with: [
			spec
				loads: 'core';
				repository: repository ].

		spec baseline: 'Beacon' with: [
			spec
				loads: #( 'CoreTests' );
				repository: repository ].

		spec baseline: 'SystemBenchmark' with: [
			spec
				loads: #( 'tests' );
				repository: repository ].

		spec baseline: 'Clap' with: [
			spec
				loads: #( 'core' );
				repository: repository ].

		spec baseline: 'TaskIt' with: [
			spec
				loads: #( 'coreTests' );
				repository: repository ].

		spec
			baseline: 'ExternalProjectLoader'
			with: [ spec repository: repository ] ]
]
