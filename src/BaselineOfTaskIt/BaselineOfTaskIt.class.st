Class {
	#name : 'BaselineOfTaskIt',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfTaskIt',
	#package : 'BaselineOfTaskIt'
}

{ #category : 'baselines' }
BaselineOfTaskIt >> baseline: spec [

	<baseline>
	spec for: #'pharo6.1.x' do: [ self baselineForPharo6: spec ].
	spec for: #'pharo7.x' do: [ self baselineForPharo7: spec ].

	spec for: (self pharoVersionsFrom: 8) do: [
		spec package: #TaskIt.
		self baselineForCommon: spec ]
]

{ #category : 'baselines' }
BaselineOfTaskIt >> baselineForCommon: spec [

	spec
		package: #'TaskIt-Tests' with: [ spec requires: #('TaskIt') ];
		package: #TaskItRetry with: [ spec requires: #('TaskIt') ];
		package: #'TaskItRetry-Tests' with: [ spec requires: #('TaskItRetry') ];
		package: #TaskItProcesses with: [ spec requires: #('TaskIt') ];
		package: #'TaskItProcesses-Tests' with: [ spec requires: #('TaskItProcesses') ];
		package: #TaskItBrowser with: [ spec requires: #('TaskItProcesses') ];
		package: #TaskItDebugger with: [ spec requires: #('TaskItProcesses') ];
		package: #'TaskItDebugger-Tests' with: [ spec requires: #('TaskItDebugger') ].

	spec
		group: 'core' with: #('TaskIt');
		group: 'coreTests' with: #('TaskIt' 'TaskIt-Tests');
		group: 'default' with: #(
			'core' 
			'TaskItProcesses' 
			'TaskItRetry' 
			'TaskItDebugger' 
			'TaskIt-Tests' 
			'TaskItRetry-Tests' 
			'TaskItProcesses-Tests' 
			'TaskItDebugger-Tests');
		group: 'debug' with: #('minimal' 'TaskItDebugger');
		group: 'tests' with: #(
			'default' 
			'TaskIt-Tests' 
			'TaskItRetry-Tests' 
			'TaskItProcesses-Tests' 
			'TaskItDebugger-Tests');
		group: 'development' with: #('default' 'debug' 'tests')
]

{ #category : 'baselines' }
BaselineOfTaskIt >> baselineForPharo6: spec [
	spec
		package: #TaskIt;
		package: #'TaskIt-Tests' with: [ spec requires: #( 'TaskIt' ) ];
		package: #TaskItRetry with: [ spec requires: #( 'TaskIt' ) ];
		package: #'TaskItRetry-Tests'
		with: [ spec requires: #( 'TaskItRetry' ) ];
		package: #TaskItProcesses with: [ spec requires: #( 'TaskIt' ) ];
		package: #'TaskItProcesses-Tests'
		with: [ spec requires: #( 'TaskItProcesses' ) ];
		package: #TaskItBrowser
		with: [ spec requires: #( 'TaskItProcesses' ) ];
		package: #TaskItDebugger
		with: [ spec requires: #( 'TaskItProcesses' ) ];
		package: #'TaskItDebugger-Tests'
		with: [ spec requires: #( 'TaskItDebugger' ) ].
	spec
		group: 'minimal' with: #( 'TaskIt' );
		group: 'default'
		with: #( 'minimal' 'TaskItProcesses' 'TaskItRetry' 'TaskItDebugger'
			   'TaskIt-Tests' 'TaskItRetry-Tests' 'TaskItProcesses-Tests'
			   'TaskItDebugger-Tests' );
		group: 'debug' with: #( 'minimal' 'TaskItDebugger' );
		group: 'tests'
		with:
			#( 'default' 'TaskIt-Tests' 'TaskItRetry-Tests' 'TaskItProcesses-Tests'
			   'TaskItDebugger-Tests' );
		group: 'development' with: #( 'default' 'debug' 'tests' )
]

{ #category : 'baselines' }
BaselineOfTaskIt >> baselineForPharo7: spec [

	spec 
		baseline: 'ParametrizedTests'
		with: [ spec repository: 'github://tesonep/ParametrizedTests/src' ].
	spec package: #TaskIt with: [ spec requires: #('ParametrizedTests') ].
	self baselineForCommon: spec
]
