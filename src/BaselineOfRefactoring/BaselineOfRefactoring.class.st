Class {
	#name : 'BaselineOfRefactoring',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfRefactoring',
	#package : 'BaselineOfRefactoring'
}

{ #category : 'baselines' }
BaselineOfRefactoring >> baseline: spec [
	"What a super strange baseline it does not load the core engine :("

	<baseline>
	spec
		package: 'Refactoring-Environment';
		package: 'Refactoring-Changes' with: [ spec requires: #( 'Refactoring-Environment' ) ];
		package: 'Refactoring-Core' with: [ spec requires: #( 'Refactoring-Changes' ) ];
		package: 'Refactoring-Transformations' with: [ spec requires: #( 'Refactoring-Core' ) ];
		package: 'Refactoring-UI' with: [ spec requires: #( 'Refactoring-Transformations') ];
		
		package: 'Refactoring-DataForTesting';
		package: 'Refactoring-UI-Tests' with: [ spec requires: #( 'Refactoring-DataForTesting' ) ];
		package: 'Refactoring-Tests-Core' with: [ spec requires: #( 'Refactoring-Core' ) ];
		package: 'Refactoring-Tests-Changes' with: [ spec requires: #( 'Refactoring-Changes' ) ];
		package: 'Refactoring-Tests-Critics';
		package: 'Refactoring-Tests-Environment' with: [ spec requires: #( 'Refactoring-Environment' ) ];
		package: 'Refactoring-Transformations-Tests' with: [ spec requires: #( 'Refactoring-Transformations' ) ];
		
		group: 'Core' with: #('Refactoring-Changes' 'Refactoring-Core' 'Refactoring-UI' 'Refactoring-Environment' 'Refactoring-Transformations');
		group: 'Tests' with: #( 'Refactoring-Tests-Core' 'Refactoring-Tests-Changes' 'Refactoring-Tests-Critics' 'Refactoring-Tests-Environment' 'Refactoring-UI-Tests'  'Refactoring-Transformations-Tests')
]
