Class {
	#name : 'BaselineOfQA',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfQA',
	#package : 'BaselineOfQA'
}

{ #category : 'baselines' }
BaselineOfQA >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [
		
		spec package: 'Refactoring-UI'.
		spec
			package: 'NautilusRefactoring'
			with: [ spec requires: #( 'Refactoring-UI' ) ].

		spec package: 'Refactoring-Critics'.
		spec baseline: 'Manifest' with: [ spec repository: repository ].

		spec
			baseline: 'QualityAssistant'
			with: [ spec repository: repository ].
		spec
			baseline: 'DependencyAnalyzer'
			with: [ spec repository: repository ].

		spec
			package: 'General-Rules'
			with: [ spec requires: #( 'QualityAssistant' ) ];
			package: 'General-Rules-Tests'
			with: [ spec requires: #( 'General-Rules' ) ];
			package: 'Specific-Rules'
			with: [ spec requires: #( 'QualityAssistant' ) ];
			package: 'Specific-Rules-Tests'
			with: [ spec requires: #( 'Specific-Rules' ) ];
			package: 'SUnit-Rules'
			with: [ spec requires: #( 'QualityAssistant' ) ];
			package: 'SUnit-Rules-Tests'
			with: [ spec requires: #( 'SUnit-Rules' ) ];
			package: 'ReleaseTests';
			package: 'NautilusRefactoring-Tests'.

		spec baseline: 'Reflectivity' with: [
			spec
				repository: repository;
				loads: #( 'tools' ) ] ]
]
