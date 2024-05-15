"
I am a resource holding a dependency analysis of the system.

I can be used by test about system dependencies to not rebuild it at each test.
"
Class {
	#name : 'SystemDependenciesReportResource',
	#superclass : 'TestResource',
	#instVars : [
		'dependenciesReport'
	],
	#category : 'System-Dependencies-Tests',
	#package : 'System-Dependencies-Tests'
}

{ #category : 'accessing' }
SystemDependenciesReportResource >> dependenciesReport [

	^ dependenciesReport ifNil: [ self rebuildDependenciesReport ]
]

{ #category : 'accessing' }
SystemDependenciesReportResource >> rebuildDependenciesReport [

	^ dependenciesReport := DADependencyChecker new computeImageDependencies
]

{ #category : 'running' }
SystemDependenciesReportResource >> tearDown [

	dependenciesReport := nil.

	super tearDown
]
