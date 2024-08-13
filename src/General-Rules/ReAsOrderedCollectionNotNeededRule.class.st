"
A prior conversion to an Array or OrderedCollection is not necessary when adding all elements to a collection.

"
Class {
	#name : 'ReAsOrderedCollectionNotNeededRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReAsOrderedCollectionNotNeededRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReAsOrderedCollectionNotNeededRule class >> ruleName [
	^ '#asOrderedCollection/#asArray not needed'
]

{ #category : 'accessing' }
ReAsOrderedCollectionNotNeededRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'AsOrderedCollectionNotNeededRule'
]

{ #category : 'initialization' }
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
