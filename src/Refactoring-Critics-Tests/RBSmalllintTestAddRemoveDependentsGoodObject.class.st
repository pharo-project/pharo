"
This class shouldn't trigger the RBAddRemoveDependentsRule critic.
"
Class {
	#name : 'RBSmalllintTestAddRemoveDependentsGoodObject',
	#superclass : 'RBSmalllintTestSuperclassObject',
	#category : 'Refactoring-Critics-Tests-Base',
	#package : 'Refactoring-Critics-Tests',
	#tag : 'Base'
}

{ #category : 'adding' }
RBSmalllintTestAddRemoveDependentsGoodObject >> addDependentSends [
	self
		addDependent: #one;
		addDependent: #two
]

{ #category : 'removing' }
RBSmalllintTestAddRemoveDependentsGoodObject >> removeDependentSends [
	self
		removeDependent: #one;
		removeDependent: #two
]
