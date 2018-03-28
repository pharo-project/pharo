Class {
	#name : #BaselineOfVersionner,
	#superclass : #BaselineOf,
	#category : #BaselineOfVersionner
}

{ #category : #baseline }
BaselineOfVersionner >> baseline: spec [
	<baseline>
	| repository |
	repository := self packageRepositoryURL.

	spec for: #common do: [
		
		spec baseline: 'SUnit' with: [spec repository: repository].
		spec baseline: 'IDE' with: [spec repository: repository].
		
		spec
			package: 'Versionner-Core-Model';
			package: 'Versionner-Core-Commands';
			package: 'Versionner-Core-Announcements';
			package: 'Versionner-Core-DependenciesModel';
			package: 'Versionner-Spec-Browser';
			package: 'Versionner-Commit'.
			
		spec
			package: 'Versionner-Tests-Resources' with: [ spec requires: #( 'SUnit' 'Core' ) ];
			package: 'Versionner-Tests-Core-Model' with: [ spec requires: #( 'SUnit' 'Core' ) ];
			package: 'Versionner-Tests-Core-Commands' with: [ spec requires: #( 'SUnit' 'Core' ) ];
			package: 'Versionner-Tests-Core-DependenciesModel' with: [ spec requires: #( 'SUnit' 'Core' ) ].
			
		spec
			group: 'Core'
			with: #(
				'Versionner-Core-Model'
				'Versionner-Core-Commands'
				'Versionner-Core-Announcements'
				'Versionner-Core-DependenciesModel'
				'Versionner-Spec-Browser'
				'Versionner-Commit').
		spec
			group: 'Tests'
			with: #(
				'Versionner-Tests-Resources'
				'Versionner-Tests-Core-Model'
				'Versionner-Tests-Core-Commands'			
				'Versionner-Tests-Core-DependenciesModel'). ]
]
