"
This class should trigger the RBAddRemoveDependentsRule critic.
"
Class {
	#name : #RBSmalllintTestAddRemoveDependentsObject,
	#superclass : #RBSmalllintTestSuperclassObject,
	#category : 'Refactoring-Tests-Critics'
}

{ #category : #adding }
RBSmalllintTestAddRemoveDependentsObject >> addDependentSends [
	self
		addDependent: #one;
		addDependent: #two
]

{ #category : #removing }
RBSmalllintTestAddRemoveDependentsObject >> removeDependentSends [
	self removeDependent: #one
]
