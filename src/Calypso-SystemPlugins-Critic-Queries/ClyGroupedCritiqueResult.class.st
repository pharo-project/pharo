"
I am critique query result which group given critiques by group name.
I represent these groups with ClyConcreteCritiqueMethodGroup instances
"
Class {
	#name : #ClyGroupedCritiqueResult,
	#superclass : #ClyQueryResult,
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #building }
ClyGroupedCritiqueResult >> buildMethodGroupWithCritique: aCritique on: resultDictionary [
	| critiqueGroupName group |
	
	critiqueGroupName := aCritique rule group.
	group := resultDictionary at: critiqueGroupName ifAbsentPut: [ 
		ClyConcreteCritiqueMethodGroup 
			basedOn: aCritique withMethodsFrom: self scope].
	group addCritique: aCritique
]

{ #category : #initialization }
ClyGroupedCritiqueResult >> defaultItems [
	^SortedCollection sortBlock: ClySortByNameFunction ascending
]

{ #category : #building }
ClyGroupedCritiqueResult >> fillWith: critiques [
	| groups |
	groups := Dictionary new.
	
	critiques do: [ :eachCritique |
		self buildMethodGroupWithCritique: eachCritique on: groups ].
	
	groups do: [ :each | items add: each  ]
]
