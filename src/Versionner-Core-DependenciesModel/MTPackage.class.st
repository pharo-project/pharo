"
A MTPackage represents an internal depedency of a project, i.e a package located in the same MCPackage.

Note: Metacello allows to describe external dependencies. In this case, transitive dependencies are not fetched. As this is not the good way to express dependencies, it is not supported here!
"
Class {
	#name : #MTPackage,
	#superclass : #MTDependency,
	#category : #Versionner-Core-DependenciesModel
}

{ #category : #visiting }
MTPackage >> acceptVisitor: anMTProjectVisitor [
	anMTProjectVisitor visitPackage: self
]

{ #category : #displaying }
MTPackage >> displayString [
	^ version ifNil:
		[ name ]
			   ifNotNil:
		[ :aVersion | name, ' (', aVersion author , '.', aVersion name asString, ')' ].
]
