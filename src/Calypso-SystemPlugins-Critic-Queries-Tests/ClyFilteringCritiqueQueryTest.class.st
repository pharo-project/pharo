Class {
	#name : #ClyFilteringCritiqueQueryTest,
	#superclass : #ClyCritiqueQueryTest,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #testing }
ClyFilteringCritiqueQueryTest class >> isAbstract [
	^self = ClyFilteringCritiqueQueryTest
]

{ #category : #tests }
ClyFilteringCritiqueQueryTest >> testComparisonWithSimilarQueryBasedOnDifferentBaseCritiqueQuery [
	| query2 |
	query baseCritiqueQuery: ClyAllBasisCritiqueQuery new.
	query2 := self createQuery.
	query2 baseCritiqueQuery: ClyAllMethodCritiqueQuery new.

	self deny: query equals: query2
]

{ #category : #tests }
ClyFilteringCritiqueQueryTest >> testHasBasisCritiquesAsBaseQuery [

	query := query class new.
	self assert: query baseCritiqueQuery class equals: ClyAllBasisCritiqueQuery 
]
