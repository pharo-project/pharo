"
A prior conversion to an Array or OrderedCollection is not necessary when adding all elements to a collection.

"
Class {
	#name : #RBAsOrderedCollectionNotNeededRule,
	#superclass : #ReNodeRewriteRule,
	#category : 'GeneralRules-Migrated'
}

{ #category : #accessing }
RBAsOrderedCollectionNotNeededRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AsOrderedCollectionNotNeededRule'
]

{ #category : #accessing }
RBAsOrderedCollectionNotNeededRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBAsOrderedCollectionNotNeededRule >> initialize [
	super initialize.
	#('addAll:' 'removeAll:' 'includesAll:' 'copyWithoutAll:')
		do: [ :collectionMessage | 
			#('asArray' 'asOrderedCollection' 'asSortedCollection')
				do: [ :conversionMessage | 
					| baseString |
					baseString := '``@receiver ' , collectionMessage , ' ``@arg '.
					self replace: baseString , conversionMessage with: baseString ] ]
]

{ #category : #accessing }
RBAsOrderedCollectionNotNeededRule >> name [
	^ '#asOrderedCollection/#asArray not needed'
]
