Class {
	#name : #TaPrecedenceComposition,
	#superclass : #TaSequence,
	#instVars : [
		'preferedTrait'
	],
	#category : #'TraitsV2-Compositions'
}

{ #category : #accessing }
TaPrecedenceComposition >> classComposition [
	^ (self class withAll: (members collect:#classComposition))
		preferedTrait: preferedTrait classComposition;
		yourself
]

{ #category : #accessing }
TaPrecedenceComposition >> copyTraitExpression [

	^ (self class withAll: (members collect:#copyTraitExpression))
		preferedTrait: preferedTrait copyTraitExpression;
		yourself
]

{ #category : #accessing }
TaPrecedenceComposition >> copyWithoutTrait: aTrait [
	| newMembers |

	newMembers := members collect: [ :e | e copyWithoutTrait: aTrait ] thenReject: #isEmpty.

	newMembers ifEmpty: [ ^ (self class withAll: newMembers) ].

	^ (self class withAll: newMembers)
		preferedTrait: (preferedTrait copyWithoutTrait: aTrait);
		yourself
	

]

{ #category : #testing }
TaPrecedenceComposition >> isConflictingSelector: aSelector [

	"If the method is in the preferedTrait it is not a conflict"
	
	(preferedTrait methods anySatisfy: [ :each | each selector = aSelector and: [ each isRequired not ] ])
		ifTrue: [ ^ false ].
		
	^ super isConflictingSelector: aSelector.
]

{ #category : #accessing }
TaPrecedenceComposition >> memberForSelector: aSelector [

	"I look first in the prefered trait if not in the other member"
	((preferedTrait selectors includes: aSelector) and: [ (preferedTrait compiledMethodAt: aSelector) isRequired not ]) ifTrue: [ ^ preferedTrait ].
	
	^ super memberForSelector: aSelector.

]

{ #category : #accessing }
TaPrecedenceComposition >> preferedTrait [
	^ preferedTrait
]

{ #category : #accessing }
TaPrecedenceComposition >> preferedTrait: anObject [
	preferedTrait := anObject
]

{ #category : #accessing }
TaPrecedenceComposition >> traitCompositionExpression [
		
	^  '(' , (super traitCompositionExpression) , ' withPrecedenceOf: ' , preferedTrait traitCompositionExpression , ')'
]
