"
Checks that all subclasses of the Collection classes that add an instance variable also redefine the copyEmpty method. This method is used when the collection grows. It copies over the necessary instance variables to the new larger collection.
"
Class {
	#name : #RBCollectionCopyEmptyRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBCollectionCopyEmptyRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBCollectionCopyEmptyRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'CollectionCopyEmptyRule'
]

{ #category : #running }
RBCollectionCopyEmptyRule >> basicCheck: aClass [
	^ (aClass inheritsFrom: Collection) and: [ aClass isVariable and: [ (aClass includesSelector: #copyEmpty) not and: [ aClass instVarNames isNotEmpty ] ] ]
]

{ #category : #accessing }
RBCollectionCopyEmptyRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBCollectionCopyEmptyRule >> name [
	^ 'Subclass of collection that has instance variable but doesn''t define copyEmpty'
]
