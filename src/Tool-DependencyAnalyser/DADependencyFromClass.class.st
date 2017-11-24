"
A PDDependencyFromClass is a dependency from the class. The source class is stored in the inst var.
"
Class {
	#name : #DADependencyFromClass,
	#superclass : #DAPackageDependency,
	#instVars : [
		'theClass'
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #comparing }
DADependencyFromClass >> <= other [
	^ self theClass <= other theClass
]

{ #category : #accessing }
DADependencyFromClass >> theClass [
	^ theClass
]

{ #category : #accessing }
DADependencyFromClass >> theClass: aClass [
	theClass := aClass
]
