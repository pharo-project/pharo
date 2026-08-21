Class {
	#name : 'BaselineOfRefactoringUI',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfRefactoringUI',
	#package : 'BaselineOfRefactoringUI'
}

{ #category : 'baselines' }
BaselineOfRefactoringUI >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [ "Here we have a dependency to spec and Commander that we may need to resolve"
			spec baseline: 'Refactoring' with: [ spec repository: repository ].
			spec
				package: 'Refactoring-UI' with: [ spec requires: #( 'Refactoring' ) ];
				package: 'Refactoring-UI-Tests' with: [ spec requires: #( 'Refactoring' 'Refactoring-UI' ) ].

			spec
				group: 'Core' with: { 'Refactoring-UI' };
				group: 'Tests' with: { 'Refactoring-UI-Tests' } ]
]
