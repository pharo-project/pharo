Class {
	#name : #BaselineOfQAGT,
	#superclass : #BaselineOf,
	#category : #BaselineOfQAGT
}

{ #category : #baselines }
BaselineOfQAGT >> baseline: spec [
	<baseline>
	
	| repository | 
	
	repository := self packageRepositoryURL.	
	
		spec for: #'common' do: [
			spec baseline: 'PharoGlamour' with: [ spec repository: repository ].
			spec baseline: 'PharoGT' with: [ spec repository: repository ].

			spec package: 'NautilusRefactoring'.
				
			spec baseline: 'SmartSuggestions' with: [ spec repository: repository ].
				
			spec package: 'Refactoring-Critics'.
			spec baseline: 'Manifest' with: [ spec repository: repository ].
			
			spec baseline: 'QualityAssistant' with: [ spec repository: repository ].
			spec baseline: 'DependencyAnalyzer' with: [ spec repository: repository ].

			spec 
				package: 'GeneralRules';
				package: 'GeneralRules-Tests';
				package: 'Kernel-Rules';
				package: 'ReleaseTests';
				package: 'Reflectivity-Tools';
				package: 'NautilusRefactoring-Tests'.
					
			spec baseline: 'Reflectivity' with: [ spec repository: repository ] ]
]
