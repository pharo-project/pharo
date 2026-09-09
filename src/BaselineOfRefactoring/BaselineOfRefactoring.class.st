Class {
	#name : 'BaselineOfRefactoring',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfRefactoring',
	#package : 'BaselineOfRefactoring'
}

{ #category : 'baselines' }
BaselineOfRefactoring >> baseline: spec [

	<baseline>
	spec package: 'Regex-Core'.
	spec
		package: 'AST-Core-Tests';
		package: 'Refactoring-Environment';
		package: 'Refactoring-Changes' with: [ spec requires: #( 'Refactoring-Environment' ) ];
		package: 'Refactoring-CompilerSupport';
		package: 'Refactoring-Core'
		with: [ spec requires: #( 'Refactoring-Changes' 'Refactoring-Environment' 'Refactoring-CompilerSupport' ) ];
		package: 'Refactoring-Conditions' with: [ spec requires: #( 'Refactoring-Core' 'Refactoring-CompilerSupport' ) ];
		package: 'Refactoring-Transformations'
		with: [ spec requires: #( 'Regex-Core' 'Refactoring-Core' 'Refactoring-Conditions' 'Refactoring-CompilerSupport' ) ];
		package: 'Refactoring-DataForTesting'
		with: [ spec requires: #( 'Refactoring-Core' 'Refactoring-Environment' 'Refactoring-Changes' ) ];
		package: 'Refactoring-CompilerSupport-Tests' with: [ spec requires: #( 'Refactoring-CompilerSupport' ) ];
		package: 'Refactoring-Core-Tests' with: [ spec requires: #( 'Refactoring-Core' 'AST-Core-Tests' 'Refactoring-DataForTesting') ];
		package: 'Refactoring-Changes-Tests' with: [ spec requires: #( 'Refactoring-Changes' ) ];
		package: 'Refactoring-Critics-Tests';
		package: 'Refactoring-Environment-Tests' with: [ spec requires: #( 'Refactoring-Environment' ) ];
		package: 'Refactoring-Transformations-Tests' with: [ spec requires: #( 'Refactoring-Transformations' ) ];
		group: 'Core'
		with:
			#( 'Refactoring-Changes' 'Refactoring-Conditions' 'Refactoring-Core' 'Refactoring-Environment' 'Refactoring-Transformations' );
		group: 'Tests'
		with: #( 'Refactoring-Core-Tests' 'Refactoring-Changes-Tests' 'Refactoring-Critics-Tests' 'Refactoring-Environment-Tests'
			   'Refactoring-Transformations-Tests' );
		group: 'default' with: #( 'Core' )
]
