Class {
	#name : 'BaselineOfRefactoring',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfRefactoring',
	#package : 'BaselineOfRefactoring'
}

{ #category : 'baselines' }
BaselineOfRefactoring >> baseline: spec [

	<baseline>
	spec
		package: 'Refactoring-Environment';
		package: 'Refactoring-Changes' with: [ spec requires: #( 'Refactoring-Environment' ) ];
		package: 'Refactoring-Core' with: [ spec requires: #( 'Refactoring-Changes' ) ];
		package: 'Refactoring-Transformations' with: [ spec requires: #( 'Refactoring-Core' ) ];
		package: 'Refactoring-UI' with: [ spec requires: #( 'Refactoring-Transformations' ) ];
		package: 'Refactoring-DataForTesting';
		package: 'Refactoring-UI-Tests' with: [ spec requires: #( 'Refactoring-DataForTesting' ) ];
		package: 'Refactoring-Core-Tests' with: [ spec requires: #( 'Refactoring-Core' ) ];
		package: 'Refactoring-Changes-Tests' with: [ spec requires: #( 'Refactoring-Changes' ) ];
		package: 'Refactoring-Critics-Tests';
		package: 'Refactoring-Environment-Tests' with: [ spec requires: #( 'Refactoring-Environment' ) ];
		package: 'Refactoring-Transformations-Tests' with: [ spec requires: #( 'Refactoring-Transformations' ) ];
		group: 'Core' with: #( 'Refactoring-Changes' 'Refactoring-Core' 'Refactoring-UI' 'Refactoring-Environment' 'Refactoring-Transformations' );
		group: 'Tests'
		with: #( 'Refactoring-Core-Tests' 'Refactoring-Changes-Tests' 'Refactoring-Critics-Tests' 'Refactoring-Environment-Tests' 'Refactoring-UI-Tests'
			   'Refactoring-Transformations-Tests' )
]
