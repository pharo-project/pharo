Class {
	#name : 'BaselineOfTaskIt',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfTaskIt',
	#package : 'BaselineOfTaskIt'
}

{ #category : 'development support' }
BaselineOfTaskIt class >> DevelopmentSupport [

"See the methods in the 'development support' category on the class-side of MetacelloBaseConfiguration. Decide what development support methods you would like to use and copy them the the class-side of your configuration."
	<apiDocumentation>
]

{ #category : 'catalog' }
BaselineOfTaskIt class >> catalogContactInfo [
	^ 'santiagobragagnolo@gmail.com'
]

{ #category : 'catalog' }
BaselineOfTaskIt class >> catalogDescription [
	^ 'Task & Jobs oriented multiprocessing framework. Check for nicely showed documentation at https://github.com/pharo-contributions/taskit/ :)'
]

{ #category : 'catalog' }
BaselineOfTaskIt class >> catalogKeywords [
	^ {'multiprocessing'. 'futures'. 'tasks'. 'services'. }
]

{ #category : 'metacello tool support' }
BaselineOfTaskIt class >> isMetacelloConfig [
	"Answer true and the Metacello tools will operate on you"
	
	^true
]

{ #category : 'loading' }
BaselineOfTaskIt class >> load [
	"Load the #stable version defined for this platform. The #stable version is the version that is recommended to be used on this platform."

	"self load"

	<apiDocumentation>
	^(self project version: #stable) load
]

{ #category : 'loading' }
BaselineOfTaskIt class >> loadBleedingEdge [
	"Load the latest versions of the mcz files defined for this project. It is not likely that the #bleedingEdge has been tested."

	"self loadBleedingEdge"

	<apiDocumentation>
	^(self project version: #bleedingEdge) load
]

{ #category : 'loading' }
BaselineOfTaskIt class >> loadDevelopment [
	"Load the #development version defined for this platform. The #development version will change over time and is not expected to be stable."

	"self loadDevelopment"

	<apiDocumentation>
	^(self project version: #development) load
]

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
