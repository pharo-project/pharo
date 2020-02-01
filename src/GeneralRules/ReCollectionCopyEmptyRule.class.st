"
Checks that all subclasses of the Collection classes that add an instance variable also redefine the copyEmpty method. This method is used when the collection grows. It copies over the necessary instance variables to the new larger collection.
"
Class {
	#name : #ReCollectionCopyEmptyRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReCollectionCopyEmptyRule class >> checksClass [
	^ true
]

{ #category : #accessing }
ReCollectionCopyEmptyRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'CollectionCopyEmptyRule'
]

{ #category : #running }
ReCollectionCopyEmptyRule >> basicCheck: aClass [
	^ (aClass inheritsFrom: Collection) and: [ aClass isVariable and: [ (aClass includesSelector: #copyEmpty) not and: [ aClass instVarNames isNotEmpty ] ] ]
]

{ #category : #accessing }
ReCollectionCopyEmptyRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReCollectionCopyEmptyRule >> name [
	^ 'Subclass of collection that has instance variable but doesn''t define copyEmpty'
]
