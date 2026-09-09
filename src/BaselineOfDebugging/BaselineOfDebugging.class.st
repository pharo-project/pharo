Class {
	#name : 'BaselineOfDebugging',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfDebugging',
	#package : 'BaselineOfDebugging'
}

{ #category : 'accessing' }
BaselineOfDebugging class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : 'baselines' }
BaselineOfDebugging >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
			spec
				baseline: 'MorphicCore' with: [
						spec
							repository: repository;
							loads: #( 'System-Object Events' ) ];
				package: 'UIManager';
				package: 'Debugging-Utils';
				package: 'Debugging-Core' with: [ spec requires: #( 'Debugging-Utils' ) ];
				package: 'Debugger-Model' with: [ spec requires: #( 'Debugging-Core' 'MorphicCore') ];
				package: 'Debugger-Oups' with: [ spec requires: #( 'Debugger-Model' ) ];
				package: 'Debugging-Utils-Tests' with: [ spec requires: #( 'Debugging-Core' ) ];
				package: 'Debugger-Model-Tests' with: [ spec requires: #( 'Debugger-Model' ) ];
				package: 'Debugger-Oups-Tests' with: [ spec requires: #( 'Debugger-Oups' ) ].

			spec
				group: 'Utils' with: #( 'Debugging-Utils' );
				group: 'Core' with: #( 'UIManager' 'Debugging-Core' 'Debugging-Utils' 'Debugger-Model' 'Debugger-Oups' );
				group: 'Tests' with: #( 'Debugging-Utils-Tests' 'Debugger-Model-Tests' 'Debugger-Oups-Tests' ) ]
]
