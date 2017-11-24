"
This class should trigger the RBClassNotReferencedRule critic. All other classes in this hierarchy must be referenced somewhere. After adding a class to the hierarchy, evaluate the following to ensure it has at least one explicit reference:
	RBSmalllintTestSuperclassObject updateDefaultExplicitClassReferences.
"
Class {
	#name : #RBSmalllintTestClassNotReferencedObject,
	#superclass : #RBSmalllintTestSuperclassObject,
	#category : #'Refactoring-Tests-Critics'
}

{ #category : #testing }
RBSmalllintTestClassNotReferencedObject class >> mustNotBeReferenced [
	^ true
]
