Class {
	#name : 'BaselineOfSystemCommands',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSystemCommands',
	#package : 'BaselineOfSystemCommands'
}

{ #category : 'baselines' }
BaselineOfSystemCommands >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
		spec baseline: 'Commander' with: [ spec repository: repository ].
		spec
			package: #'SystemCommands-RefactoringSupport'
			with: [ spec requires: #( 'Commander' ) ];
			package: #'SystemCommands-ClassCommands' with: [
				spec requires:
						#( 'Commander' #'SystemCommands-RefactoringSupport' ) ];
			package: #'SystemCommands-MessageCommands' with: [
				spec requires:
						#( 'Commander' #'SystemCommands-RefactoringSupport' ) ];
			package: #'SystemCommands-MethodCommands' with: [
				spec requires:
						#( 'Commander' #'SystemCommands-RefactoringSupport' ) ];
			package: #'SystemCommands-MethodCommands-Tests'
			with: [ spec requires: #( #'SystemCommands-MethodCommands' ) ];
			package: #'SystemCommands-PackageCommands'
			with: [ spec requires: #( 'Commander' ) ];
			package: #'SystemCommands-PackageCommands-Tests'
			with: [ spec requires: #( #'SystemCommands-PackageCommands' ) ];
			package: #'SystemCommands-SourceCodeCommands' with: [
				spec requires:
						#( 'Commander' #'SystemCommands-RefactoringSupport' ) ];
			package: #'SystemCommands-VariableCommands' with: [
				spec requires:
						#( 'Commander' #'SystemCommands-RefactoringSupport' ) ];
			package: #'SystemCommands-RefactoringSupport-Tests'
			with: [ spec requires: #( #'SystemCommands-RefactoringSupport' ) ] ]
]
