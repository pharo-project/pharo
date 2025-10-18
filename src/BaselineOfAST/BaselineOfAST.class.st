Class {
	#name : 'BaselineOfAST',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfAST',
	#package : 'BaselineOfAST'
}

{ #category : 'baselines' }
BaselineOfAST >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec baseline: 'SUnit' with: [
			spec
				repository: (self packageRepositoryURLForSpec: spec);
				loads: 'SUnit-Core' ].
		spec
			package: 'AST-Core';
			package: 'AST-Core-Tests'
			with: [ spec requires: #( 'SUnit' 'AST-Core' ) ].

		spec
			group: 'default' with: #( 'AST-Core' );
			group: 'tests' with: #( 'AST-Core-Tests' ) ]
]
