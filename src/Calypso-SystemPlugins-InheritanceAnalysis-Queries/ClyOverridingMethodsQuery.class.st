"
I return all method from given scope which override methods from inherited classes.

To detect overriding method I use inheritanceAnalyzer which caches this information
"
Class {
	#name : #ClyOverridingMethodsQuery,
	#superclass : #ClyInheritanceAnalysisMethodQuery,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #printing }
ClyOverridingMethodsQuery >> description [

	^'overriding methods'
]

{ #category : #'system changes' }
ClyOverridingMethodsQuery >> isAffectedByChangedMethod: aMethod [
	| selector |
	
	(self isFromScopeWithClassKindOf: aMethod origin) ifFalse: [ ^false ].
	
	selector := aMethod selector.
	scope classesDo: [ :each | 
		(each lookupSelector: selector) ifNotNil: [ ^true ]].
	^ false
]

{ #category : #testing }
ClyOverridingMethodsQuery >> selectsMethod: aMethod [
	^scope inheritanceAnalyzer isMethodOverriding: aMethod
]
