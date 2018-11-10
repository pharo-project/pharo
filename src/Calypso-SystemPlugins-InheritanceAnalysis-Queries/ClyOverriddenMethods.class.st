"
I return all method from given scope which are overridden by any subclasses.

To detect overridden method I use inheritanceAnalyzer which caches this information
"
Class {
	#name : #ClyOverriddenMethods,
	#superclass : #ClyInheritanceAnalysisMethodQuery,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #printing }
ClyOverriddenMethods >> description [

	^'overridden methods'
]

{ #category : #'system changes' }
ClyOverriddenMethods >> isAffectedByChangedMethod: aMethod [
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
ClyOverriddenMethods >> selectsMethod: aMethod [
	^scope inheritanceAnalyzer isMethodOverridden: aMethod
]
