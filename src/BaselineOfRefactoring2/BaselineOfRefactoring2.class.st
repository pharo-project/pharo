Class {
	#name : #BaselineOfRefactoring2,
	#superclass : #BaselineOf,
	#category : #BaselineOfRefactoring2
}

{ #category : #baselines }
BaselineOfRefactoring2 >> baseline: spec [
	<baseline>
	
	"this baseline is a bit draft it does not express real dependency between packages."
	spec 
		package: 'Refactoring-Changes';
		package: 'Refactoring2-Transformations';
		package: 'Refactoring-DataForTesting';
		package: 'Refactoring2-Transformations-Tests'.
	spec 
		group: 'Core' with: #('Refactoring-Changes' 'Refactoring2-Transformations');
		group: 'Tests' with: #('Refactoring-DataForTesting' 'Refactoring2-Transformations-Tests');
		group: 'default' with: #('Core' 'Tests')
]
