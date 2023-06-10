Class {
	#name : #BaselineOfRefactoring,
	#superclass : #BaselineOf,
	#category : #BaselineOfRefactoring
}

{ #category : #baselines }
BaselineOfRefactoring >> baseline: spec [
	<baseline>

	"What a super strange baseline it does not load the core engine :("
	
		spec 
			package: 'SUnit-Rules';
			package: 'SUnit-Rules-Tests' with: [ spec requires: #( 'SUnit-Rules' ) ];
			package: 'Kernel-Tests-Rules';
			package: 'Polymorph-Widgets-Rules';
			package: 'System-Settings-Rules';
			package: 'GeneralRules';
			package: 'Rubric-Rules';
			package: 'Refactoring-DataForTesting';
			package: 'Refactoring-Tests-Core';
			package: 'Refactoring-Tests-Changes';
			package: 'Refactoring-Tests-Critics';
			package: 'Refactoring-Tests-Environment'.
		spec 
			group: 'Rules' with: #('SUnit-Rules' 'Kernel-Tests-Rules' 'Polymorph-Widgets-Rules' 'System-Settings-Rules' 'GeneralRules' 'Rubric-Rules');
			group: 'Tests' with: #('Refactoring-Tests-Core' 'Refactoring-Tests-Changes' 'Refactoring-Tests-Critics' 'Refactoring-Tests-Environment' 'SUnit-Rules-Tests');
			group: 'default' with: #('Rules' 'Tests') 
]
