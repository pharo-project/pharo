"
I am an abstraction in the concept of projects. I can return the projects in an environment.

Since we do not have yet a real project reification we are defining projects and the content of a baseline.

I can be sconfigured with an environment to look for the projects.
"
Class {
	#name : 'ProjectManager',
	#superclass : 'Object',
	#instVars : [
		'environment'
	],
	#category : 'Tools',
	#package : 'Tools'
}

{ #category : 'accessing' }
ProjectManager >> environment [

	^ environment ifNil: [ environment := self class environment ]
]

{ #category : 'accessing' }
ProjectManager >> environment: anObject [

	environment := anObject
]

{ #category : 'testing' }
ProjectManager >> hasProjects [
	"Even if we do not have projects we have un undefined project."

	^ true
]

{ #category : 'accessing' }
ProjectManager >> projects [
	"Maybe we should cache this and invalidate the cache when a package announcement or class (impacting baselines) is received?"

	| projects packages |
	projects := self environment allClasses
		            select: [ :class | (class inheritsFrom: BaselineOf) and: [ class isProject ] ]
		            thenCollect: [ :baseline | Project baseline: baseline ].

	packages := projects flatCollect: [ :project | project packages ].

	(self environment organization packages reject: [ :package | package isUndefined or: [ packages identityIncludes: package ] ]) ifNotEmpty: [ :orphans |
		projects add: (UndefinedProject packages: orphans) ].

	^ projects
]
