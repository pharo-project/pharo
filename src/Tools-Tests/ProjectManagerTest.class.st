Class {
	#name : 'ProjectManagerTest',
	#superclass : 'TestCase',
	#category : 'Tools-Tests',
	#package : 'Tools-Tests'
}

{ #category : 'tests' }
ProjectManagerTest >> testPackagesInAProject [

	| sUnitProject |
	sUnitProject := ProjectManager new projects detect: [ :project | project name = #SUnit ].

	self
		assertCollection:
			#( 'BaselineOfSUnit' 'SUnit-Core' 'SUnit-Core-Traits' 'SUnit-Tests' 'SUnit-Visitor' 'SUnit-Visitor-Tests' 'SUnit-MockObjects' 'SUnit-MockObjects-Tests'
			   'SUnit-UI' 'Coverage' 'JenkinsTools-Core' 'JenkinsTools-ExtraReports' 'SUnit-Support-UITesting-Tests' 'SUnit-Support-UITesting' 'Coverage-Tests')
		hasSameElements: (sUnitProject packages collect: [ :package | package name ])
]

{ #category : 'tests' }
ProjectManagerTest >> testProjects [

	| manager projects someBaselines |
	manager := ProjectManager new.
	projects := manager projects.
	self denyEmpty: projects.

	someBaselines := #( #BaselineOfPharo #BaselineOfSUnit ).

	someBaselines do: [ :baseline |
		manager environment at: baseline ifAbsent: [ self fail: 'The test is expecting this class to be in the system to work.' ].
		self assert: (projects anySatisfy: [ :project | project baseline name = baseline ]) ].

	self assert: (#( Pharo SUnit ) allSatisfy: [ :projectName | projects anySatisfy: [ :project | project name = projectName ] ])
]
