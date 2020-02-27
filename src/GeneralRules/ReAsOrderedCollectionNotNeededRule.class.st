"
A prior conversion to an Array or OrderedCollection is not necessary when adding all elements to a collection.

"
Class {
	#name : #ReAsOrderedCollectionNotNeededRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReAsOrderedCollectionNotNeededRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AsOrderedCollectionNotNeededRule'
]

{ #category : #accessing }
ReAsOrderedCollectionNotNeededRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
ReAsOrderedCollectionNotNeededRule >> initialize [
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
ReAsOrderedCollectionNotNeededRule >> name [
	^ '#asOrderedCollection/#asArray not needed'
]
