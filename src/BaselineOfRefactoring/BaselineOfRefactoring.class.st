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
		package: 'Refactoring-DataForTesting';
		package: 'Refactoring-UI-Tests' with: [ spec requires: #( 'Refactoring-DataForTesting' ) ];
		package: 'Refactoring-Tests-Core';
		package: 'Refactoring-Tests-Changes';
		package: 'Refactoring-Tests-Critics';
		package: 'Refactoring-Tests-Environment';
		
		group: 'Tests' with: #( 'Refactoring-Tests-Core' 'Refactoring-Tests-Changes' 'Refactoring-Tests-Critics' 'Refactoring-Tests-Environment'
		          'Refactoring-UI-Tests' )
]
