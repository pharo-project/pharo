"
This smell arises when a method is equivalent to its superclass method. The methods are equivalent when they have the same abstract syntax tree, except for variables names. Such method does not add anything to the computation and can be removed since the superclass method have the same behaviour. 

Furthermore, the methods #new and #initialize are ignored once they are often overridden for compatilbity with other platforms. The ignored methods can be edited in RBEquivalentSuperclassMethodsRule>>ignoredSelectors
"
Class {
	#name : #ReEquivalentSuperclassMethodsRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReEquivalentSuperclassMethodsRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReEquivalentSuperclassMethodsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'EquivalentSuperclassMethodsRule'
]

{ #category : #running }
ReEquivalentSuperclassMethodsRule >> basicCheck: aMethod [
	(self ignoredSelectors includes: aMethod selector) ifTrue: [ ^ false ].

	^ aMethod methodClass superclass
		ifNotNil: [ :superclass | (superclass lookupSelector: aMethod selector)
			ifNotNil: [ :overridenMethod | aMethod ast = overridenMethod ast ]
			ifNil: [ false ] ]
		ifNil: [ false ]
]

{ #category : #accessing }
ReEquivalentSuperclassMethodsRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
ReEquivalentSuperclassMethodsRule >> ignoredSelectors [
	"These methods are often overridden for compatilbity with other platforms."

	^ #(new initialize)
]

{ #category : #accessing }
ReEquivalentSuperclassMethodsRule >> name [
	^ 'Methods equivalently defined in superclass'
]

{ #category : #accessing }
ReEquivalentSuperclassMethodsRule >> rationale [
	^ 'Check for methods that are equivalent to their superclass methods.'
]
