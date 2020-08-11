"
Check that the number of addDependent: message sends in a class is less than or equal to the number of removeDependent: messages. If there are more addDependent: messages that may signify that some dependents are not being released, which may lead to memory leaks.
"
Class {
	#name : #ReAddRemoveDependentsRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReAddRemoveDependentsRule class >> checksClass [
	^ true
]

{ #category : #accessing }
ReAddRemoveDependentsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AddRemoveDependentsRule'
]

{ #category : #running }
ReAddRemoveDependentsRule >> basicCheck: aClass [
	| methods addSends removeSends |
	addSends := 0.
	removeSends := 0.
	methods := (aClass whichMethodsReferTo: #addDependent:) asSet, (aClass whichMethodsReferTo: #removeDependent:).
	
	methods do: [ :method | | messages |
			messages := method ast allChildren select: [ :each | each isMessage ].
				addSends := addSends + (messages count: [ :each | each selector == #addDependent:]).
				removeSends := removeSends + (messages count: [ :each | each selector == #removeDependent:])].
	^ addSends > removeSends
]

{ #category : #accessing }
ReAddRemoveDependentsRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReAddRemoveDependentsRule >> name [
	^ 'Number of addDependent: messages > removeDependent:'
]
