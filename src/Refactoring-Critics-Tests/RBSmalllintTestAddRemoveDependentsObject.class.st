"
This class should trigger the RBAddRemoveDependentsRule critic.
"
Class {
	#name : 'RBSmalllintTestAddRemoveDependentsObject',
	#superclass : 'RBSmalllintTestSuperclassObject',
	#category : 'Refactoring-Critics-Tests-Base',
	#package : 'Refactoring-Critics-Tests',
	#tag : 'Base'
}

{ #category : 'adding' }
RBSmalllintTestAddRemoveDependentsObject >> addDependentSends [
	self
		addDependent: #one;
		addDependent: #two
]

{ #category : 'removing' }
RBSmalllintTestAddRemoveDependentsObject >> removeDependentSends [
	self removeDependent: #one
]
