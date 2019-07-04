"
Check that the number of addDependent: message sends in a class is less than or equal to the number of removeDependent: messages. If there are more addDependent: messages that may signify that some dependents are not being released, which may lead to memory leaks.
"
Class {
	#name : #RBAddRemoveDependentsRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBAddRemoveDependentsRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBAddRemoveDependentsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AddRemoveDependentsRule'
]

{ #category : #running }
RBAddRemoveDependentsRule >> basicCheck: aClass [
	| addSends removeSends |
	addSends := 0.
	removeSends := 0.
	(aClass whichSelectorsReferTo: #addDependent:) asSet
		addAll: (aClass whichSelectorsReferTo: #removeDependent:);
		do: [ :sel | 
			(aClass compiledMethodAt: sel) ast
				nodesDo: [ :each | 
					each isMessage
						ifTrue: [ each selector = #addDependent: ifTrue: [ addSends := addSends + 1 ].
							each selector = #removeDependent: ifTrue: [ removeSends := removeSends + 1 ] ] ] ].
	^ addSends > removeSends
]

{ #category : #accessing }
RBAddRemoveDependentsRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBAddRemoveDependentsRule >> name [
	^ 'Number of addDependent: messages > removeDependent:'
]
