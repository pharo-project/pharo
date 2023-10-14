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
	
		package: 'Refactoring-Changes';
		package: 'Refactoring-Core';
		package: 'Refactoring-UI';
		package: 'Refactoring-Environment';
		package: 'Refactoring2-Transformations';
		
		package: 'Refactoring-DataForTesting';
		package: 'Refactoring-UI-Tests' with: [ spec requires: #( 'Refactoring-DataForTesting' ) ];
		package: 'Refactoring-Tests-Core';
		package: 'Refactoring-Tests-Changes';
		package: 'Refactoring-Tests-Critics';
		package: 'Refactoring-Tests-Environment';
		package: 'Refactoring2-Transformations-Tests';
		
		group: 'Core' with: #('Refactoring-Changes' 'Refactoring-Core' 'Refactoring-UI' 'Refactoring-Environment' 'Refactoring2-Transformations');
		group: 'Tests' with: #( 'Refactoring-Tests-Core' 'Refactoring-Tests-Changes' 'Refactoring-Tests-Critics' 'Refactoring-Tests-Environment' 'Refactoring-UI-Tests'  'Refactoring2-Transformations-Tests')
]
