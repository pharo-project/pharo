"
This class shouldn't trigger the RBAddRemoveDependentsRule critic.
"
Class {
	#name : #RBSmalllintTestAddRemoveDependentsGoodObject,
	#superclass : #RBSmalllintTestSuperclassObject,
	#category : #'Refactoring-Tests-Critics'
}

{ #category : #adding }
RBSmalllintTestAddRemoveDependentsGoodObject >> addDependentSends [
	self
		addDependent: #one;
		addDependent: #two
]

{ #category : #removing }
RBSmalllintTestAddRemoveDependentsGoodObject >> removeDependentSends [
	self
		removeDependent: #one;
		removeDependent: #two
]
