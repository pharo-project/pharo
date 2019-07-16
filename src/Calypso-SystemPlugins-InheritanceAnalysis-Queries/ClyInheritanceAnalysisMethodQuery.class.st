"
I am abstract method query to group together other queries which analyzes methods inheritance.

I provide implementation to react on system changes and some helper methods for subclasses
"
Class {
	#name : #ClyInheritanceAnalysisMethodQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #testing }
ClyInheritanceAnalysisMethodQuery >> isFromScopeWithClassKindOf: aClass [
	scope
		classesDo: [ :each | 
			(each includesBehavior: aClass)
				ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #testing }
ClyInheritanceAnalysisMethodQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	"This query is not restricted by the scope
	because it analyses methods from scope classes together with all superclasses.
	So the result can be affected by the changes out of query scope.
	And here we omit scope logic and rely completely on event.
	So the actual decision about affect 
	is based on #isAffectedByChangedMethod: method
	and generally on any class change"
	^aSystemAnnouncement affectsClasses 
		or: [ aSystemAnnouncement canAffectResultOfMethodQuery: self]
]
