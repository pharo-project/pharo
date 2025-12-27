Class {
	#name : 'BaselineOfQA',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfQA',
	#package : 'BaselineOfQA'
}

{ #category : 'baselines' }
BaselineOfQA >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec repository: (self packageRepositoryURLForSpec: spec).
		
		spec package: 'Refactoring-UI'.
		spec package: 'NautilusRefactoring' with: [ spec requires: #( 'Refactoring-UI' ) ].
		spec package: 'Refactoring-Critics'.
		spec baseline: 'Manifest'.

		spec baseline: 'QualityAssistant'.
		spec
			package: 'General-Rules' with: [ spec requires: #( 'QualityAssistant' ) ];
			package: 'General-Rules-Tests' with: [ spec requires: #( 'General-Rules' ) ];
			package: 'Specific-Rules' with: [ spec requires: #( 'QualityAssistant' ) ];
			package: 'Specific-Rules-Tests' with: [ spec requires: #( 'Specific-Rules' ) ];
			package: 'SUnit-Rules' with: [ spec requires: #( 'QualityAssistant' ) ];
			package: 'SUnit-Rules-Tests' with: [ spec requires: #( 'SUnit-Rules' ) ];
			package: 'ReleaseTests';
			package: 'NautilusRefactoring-Tests'.

		spec baseline: 'Reflectivity' with: [ spec loads: #( 'tools' ) ] ]
]
