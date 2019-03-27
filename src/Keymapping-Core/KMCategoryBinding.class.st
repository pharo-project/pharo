"
I am the reification of the relation morph<->category, created when a morph gets attached a category of shortcuts.
"
Class {
	#name : #KMCategoryBinding,
	#superclass : #Object,
	#instVars : [
		'target',
		'category',
		'morph'
	],
	#category : #'Keymapping-Core-Categories'
}

{ #category : #'instance creation' }
KMCategoryBinding class >> target: aTarget morph: aMorph category: aCategory [
	^self new
		target: aTarget;
		category: aCategory;
		morph: aMorph;
		yourself
]

{ #category : #accessing }
KMCategoryBinding >> category [

	^ category
]

{ #category : #accessing }
KMCategoryBinding >> category: aCategory [
	category := aCategory
]

{ #category : #matching }
KMCategoryBinding >> completeMatch: aKeymap buffer: aBuffer [
	KMLog logCompleteMatch: aKeymap.
	KMBuffer uniqueInstance completeMatch.
	aKeymap action cull: target value cull: morph cull: aBuffer first.

]

{ #category : #binding }
KMCategoryBinding >> keymapForShortcut: aKey [
	
	^ category keymapForShortcut: aKey
]

{ #category : #accessing }
KMCategoryBinding >> morph [

	^ morph
]

{ #category : #accessing }
KMCategoryBinding >> morph: aMorph [
	morph := aMorph
]

{ #category : #iterating }
KMCategoryBinding >> nextForKmChain: aKMDispatchChain [
	^self target
]

{ #category : #matching }
KMCategoryBinding >> noMatch [
	"do nothing"
	
]

{ #category : #matching }
KMCategoryBinding >> partialMatch [
	"do nothing"
	
]

{ #category : #printing }
KMCategoryBinding >> printOn: aStream [

	aStream
		nextPutAll: 'aKMCategoryTarget(';
		nextPutAll: (category name ifNil: 'nil' ifNotNil: [ :n | n printString ]);
		nextPutAll: ')'.
]

{ #category : #accessing }
KMCategoryBinding >> target [

	^ target
]

{ #category : #accessing }
KMCategoryBinding >> target: aTarget [
	target := aTarget
]

{ #category : #matching }
KMCategoryBinding >> verifyMatchWith: anEventBuffer notifying: aListener thenDoing: anAction [
	self flag: #pharoFixMe. "ugly hack with array of listeners"
	category 
		onMatchWith:  anEventBuffer
		notify: { self . aListener } " first we have to tell the dispatcher "
		andDo: anAction
]
