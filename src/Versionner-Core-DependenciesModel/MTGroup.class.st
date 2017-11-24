"
A MTGroup is a convenient way to group software dependencies (instances of MTDependency). 

Instance Variables
	- dependencies : A collection of dependencies defiing the group.

"
Class {
	#name : #MTGroup,
	#superclass : #MTDependency,
	#category : #'Versionner-Core-DependenciesModel'
}

{ #category : #visiting }
MTGroup >> acceptVisitor: anMTProjectVisitor [
	anMTProjectVisitor visitGroup: self
]

{ #category : #protocol }
MTGroup >> addDependency: aDependency [
	super addDependency: aDependency.
	self project ifNotNil: [ self project announcer announce: (MTProjectModelChanged project: self) ].
]

{ #category : #displaying }
MTGroup >> displayString [
	^ name
]
