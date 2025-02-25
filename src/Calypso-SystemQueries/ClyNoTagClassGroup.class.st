"
I represent group of classes which does not marked by any tag.

I provide simple method to tag all my classes:

	aGroup renameClassTagTo: aSymbol
	
It is polymorphic to ClyTaggedClassGroup which performs actual tag renaming.

My class query is ClyRestUntaggedClasses 
"
Class {
	#name : 'ClyNoTagClassGroup',
	#superclass : 'ClyClassGroup',
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'testing' }
ClyNoTagClassGroup class >> isBasedOnPackageTag [
	^true
]

{ #category : 'accessing' }
ClyNoTagClassGroup class >> priority [
	^10
]

{ #category : 'operations' }
ClyNoTagClassGroup >> renameTagTo: newTag [

	self classes do: [ :class | class packageTag: newTag ]
]

{ #category : 'accessing' }
ClyNoTagClassGroup >> tag [
	"Uncategorized classes are going to the root tag."

	^ Package rootTagName
]
