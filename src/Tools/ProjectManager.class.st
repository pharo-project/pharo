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

	^ self projects isNotEmpty
]

{ #category : 'testing' }
ProjectManager >> isNotEmpty [

	^ self projects isNotEmpty
]

{ #category : 'accessing' }
ProjectManager >> projects [

	^ self environment allClasses
		  select: [ :class | class inheritsFrom: BaselineOf ]
		  thenCollect: [ :baseline | Project baseline: baseline ]
]
