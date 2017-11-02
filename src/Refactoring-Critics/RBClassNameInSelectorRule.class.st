"
This smell arises when the class name is found in a selector. This is redundant since to call the you must already refer to the class name. For example, #openHierarchyBrowserFrom: is a redundant name for HierarchyBrowser.
"
Class {
	#name : #RBClassNameInSelectorRule,
	#superclass : #RBBlockLintRule,
	#category : 'Refactoring-Critics-BlockRules'
}

{ #category : #testing }
RBClassNameInSelectorRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBClassNameInSelectorRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ClassNameInSelectorRule'
]

{ #category : #running }
RBClassNameInSelectorRule >> basicCheck: aMethod [ 
	(aMethod methodClass isMeta) ifFalse: [ ^ false ].
	^ (aMethod selector 
		indexOfSubCollection: aMethod methodClass soleInstance name
		startingAt: 1) > 0
]

{ #category : #running }
RBClassNameInSelectorRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) ifTrue: 
		[ result addMethod: aMethod ]
]

{ #category : #accessing }
RBClassNameInSelectorRule >> group [
	^ 'Style'
]

{ #category : #accessing }
RBClassNameInSelectorRule >> name [
	^ 'Redundant class name in selector'
]

{ #category : #accessing }
RBClassNameInSelectorRule >> rationale [
	^ 'Checks for the class name in a selector. This is redundant since to call the you must already refer to the class name. For example, openHierarchyBrowserFrom: is a redundant name for HierarchyBrowser. Avoiding selector including class name gives a chance to have more polymorphic methods.'
]
