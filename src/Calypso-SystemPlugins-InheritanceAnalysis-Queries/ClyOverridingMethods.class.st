"
I return all method from given scope which override methods from inherited classes.

To detect overriding method I use inheritanceAnalyzer which caches this information
"
Class {
	#name : #ClyOverridingMethods,
	#superclass : #ClyInheritanceAnalysisMethodQuery,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #printing }
ClyOverridingMethods >> description [

	^'overriding methods'
]

{ #category : #'system changes' }
ClyOverridingMethods >> isAffectedByChangedMethod: aMethod [
	| selector |
	
	(self isFromScopeWithClassKindOf: aMethod origin) ifFalse: [ ^false ].
	
	selector := aMethod selector.
	scope classesDo: [ :each | 
		(each lookupSelector: selector) ifNotNil: [ ^true ]].
	^ false
]

{ #category : #testing }
ClyOverridingMethods >> selectsMethod: aMethod [
	^scope inheritanceAnalyzer isMethodOverriding: aMethod
]
