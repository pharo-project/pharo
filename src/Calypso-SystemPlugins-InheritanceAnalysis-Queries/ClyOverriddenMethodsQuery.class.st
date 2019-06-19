"
I return all method from given scope which are overridden by any subclasses.

To detect overridden method I use inheritanceAnalyzer which caches this information
"
Class {
	#name : #ClyOverriddenMethodsQuery,
	#superclass : #ClyInheritanceAnalysisMethodQuery,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #printing }
ClyOverriddenMethodsQuery >> description [

	^'overridden methods'
]

{ #category : #'system changes' }
ClyOverriddenMethodsQuery >> isAffectedByChangedMethod: aMethod [
	| selector methodClass |
	selector := aMethod selector.
	methodClass := aMethod origin.
	scope classesDo: [ :each | 
		each = methodClass ifTrue: [ ^true ].		
		((methodClass inheritsFrom: each) and: [ each includesLocalSelector: selector ])
			 ifTrue: [ ^true]
	].	
	^false
]

{ #category : #testing }
ClyOverriddenMethodsQuery >> selectsMethod: aMethod [
	^scope inheritanceAnalyzer isMethodOverridden: aMethod
]
