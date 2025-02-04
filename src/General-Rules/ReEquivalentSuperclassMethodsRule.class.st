"
This smell arises when a method is equivalent to its superclass method. The methods are equivalent when they have the same abstract syntax tree, except for variables names. Such method does not add anything to the computation and can be removed since the superclass method have the same behaviour. 

Furthermore, the methods #new and #initialize are ignored once they are often overridden for compatilbity with other platforms. The ignored methods can be edited in RBEquivalentSuperclassMethodsRule>>ignoredSelectors
"
Class {
	#name : 'ReEquivalentSuperclassMethodsRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'testing' }
ReEquivalentSuperclassMethodsRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReEquivalentSuperclassMethodsRule class >> group [
	^ self designFlawGroup
]

{ #category : 'accessing' }
ReEquivalentSuperclassMethodsRule class >> rationale [
	^ 'Check for methods that are equivalent to their superclass methods.'
]

{ #category : 'accessing' }
ReEquivalentSuperclassMethodsRule class >> ruleName [
	^ 'Methods equivalently defined in superclass'
]

{ #category : 'accessing' }
ReEquivalentSuperclassMethodsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'EquivalentSuperclassMethodsRule'
]

{ #category : 'running' }
ReEquivalentSuperclassMethodsRule >> basicCheck: aMethod [

	(self ignoredSelectors includes: aMethod selector) ifTrue: [ ^ false ].

	^ aMethod hasEquivalentMethodInSuperclass
]

{ #category : 'accessing' }
ReEquivalentSuperclassMethodsRule >> ignoredSelectors [
	"These methods are often overridden for compatilbity with other platforms."

	^ #(new initialize)
]
