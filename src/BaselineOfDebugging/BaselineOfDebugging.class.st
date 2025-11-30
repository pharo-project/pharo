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
	spec for: #common do: [
			spec
				package: 'Debugging-Core';
				package: 'Debugging-Utils' with: [ spec requires: #( 'Debugging-Core' ) ];
				package: 'Debugger-Model' with: [ spec requires: #( 'Debugging-Core' 'Debugging-Utils' ) ];
				package: 'Debugger-Oups' with: [ spec requires: #( 'Debugger-Model' ) ];
				package: 'Debugging-Utils-Tests' with: [ spec requires: #( 'Debugging-Utils' ) ];
				package: 'Debugger-Model-Tests' with: [ spec requires: #( 'Debugger-Model' ) ];
				package: 'Debugger-Oups-Tests' with: [ spec requires: #( 'Debugger-Oups' ) ].

			spec
				group: 'Core' with: #( 'Debugging-Core' 'Debugging-Utils' 'Debugger-Model' 'Debugger-Oups' );
				group: 'Tests' with: #( 'Debugging-Utils-Tests' 'Debugger-Model-Tests' 'Debugger-Oups-Tests' ) ]
]
