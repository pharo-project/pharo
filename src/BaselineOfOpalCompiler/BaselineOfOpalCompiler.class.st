Class {
	#name : 'BaselineOfOpalCompiler',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfOpalCompiler',
	#package : 'BaselineOfOpalCompiler'
}

{ #category : 'baselines' }
BaselineOfOpalCompiler >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec baseline: 'SUnit' with: [
			spec
				repository: (self packageRepositoryURLForSpec: spec);
				loads: 'Core' ].
		spec
			baseline: 'AST'
			with: [ spec repository: (self packageRepositoryURLForSpec: spec) ].

		spec
			package: 'OpalCompiler-Core' with: [ spec requires: #( 'AST' ) ];
			package: 'OpalCompiler-Tests'
			with: [ spec requires: #( 'SUnit' 'OpalCompiler-Core' ) ].

		spec
			group: 'default' with: #( 'OpalCompiler-Core' );
			group: 'tests' with: #( 'OpalCompiler-Tests' ) ]
]
