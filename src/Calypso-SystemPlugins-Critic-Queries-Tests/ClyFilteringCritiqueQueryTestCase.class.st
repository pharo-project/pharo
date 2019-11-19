Class {
	#name : #ClyFilteringCritiqueQueryTestCase,
	#superclass : #ClyCritiqueQueryTestCase,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #testing }
ClyFilteringCritiqueQueryTestCase class >> isAbstract [
	^self = ClyFilteringCritiqueQueryTestCase
]

{ #category : #tests }
ClyFilteringCritiqueQueryTestCase >> testComparisonWithSimilarQueryBasedOnDifferentBaseCritiqueQuery [
	| query2 |
	query baseCritiqueQuery: ClyAllBasisCritiqueQuery new.
	query2 := self createQuery.
	query2 baseCritiqueQuery: ClyAllMethodCritiqueQuery new.

	self deny: query equals: query2
]

{ #category : #tests }
ClyFilteringCritiqueQueryTestCase >> testHasBasisCritiquesAsBaseQuery [

	query := query class new.
	self assert: query baseCritiqueQuery class equals: ClyAllBasisCritiqueQuery 
]
