"
I am the reification of the relation morph<->category, created when a morph gets attached a category of shortcuts.
"
Class {
	#name : #KMCategoryTarget,
	#superclass : #Object,
	#instVars : [
		'target',
		'category',
		'morph'
	],
	#category : #Deprecated10
}

{ #category : #testing }
KMCategoryTarget class >> isDeprecated [
	^true
]

{ #category : #'instance creation' }
KMCategoryTarget class >> target: aTarget morph: aMorph category: aCategory [
	^self new
		target: aTarget;
		category: aCategory;
		morph: aMorph;
		yourself
]

{ #category : #accessing }
KMCategoryTarget >> category [

	^ category
]

{ #category : #accessing }
KMCategoryTarget >> category: aCategory [
	category := aCategory
]

{ #category : #matching }
KMCategoryTarget >> completeMatch: aKeymap buffer: aBuffer [
	KMLog logCompleteMatch: aKeymap.
	KMBuffer uniqueInstance completeMatch.
	aKeymap action cull: target value cull: morph cull: aBuffer first.
]

{ #category : #accessing }
KMCategoryTarget >> morph [

	^ morph
]

{ #category : #accessing }
KMCategoryTarget >> morph: aMorph [
	morph := aMorph
]

{ #category : #iterating }
KMCategoryTarget >> nextForKmChain: aKMDispatchChain [
	^self target
]

{ #category : #matching }
KMCategoryTarget >> noMatch [
	"do nothing"
]

{ #category : #matching }
KMCategoryTarget >> partialMatch [
	"do nothing"
]

{ #category : #printing }
KMCategoryTarget >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $(;
		print: category name;
		nextPut: $).
]

{ #category : #accessing }
KMCategoryTarget >> target [

	^ target
]

{ #category : #accessing }
KMCategoryTarget >> target: aTarget [
	target := aTarget
]

{ #category : #matching }
KMCategoryTarget >> verifyMatchWith: anEventBuffer notifying: aListener thenDoing: anAction [
	self flag: #pharoFixMe. "ugly hack with array of listeners"
	category 
		onMatchWith:  anEventBuffer
		notify: { self . aListener } " first we have to tell the dispatcher "
		andDo: anAction
]
