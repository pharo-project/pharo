Class {
	#name : 'BaselineOfDebuggerCLI',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfDebuggerCLI',
	#package : 'BaselineOfDebuggerCLI'
}

{ #category : 'baselines' }
BaselineOfDebuggerCLI >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
			spec
				baseline: 'Debugging' with: [
						spec
							repository: repository;
							loads: #( 'Debugger-Model' ) ];
				package: 'Debugger-CLI' with: [ spec requires: #( 'Debugging' ) ];
				package: 'Debugger-CLI-Tests' with: [ spec requires: #( 'Debugger-CLI' 'Debugging' ) ].

			spec
				group: 'Core' with: { 'Debugger-CLI' };
				group: 'Tests' with: { 'Debugger-CLI-Tests' } ]
]
