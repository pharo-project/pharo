"
I am a method group which include all methods with critiques.

Also I am expanded to critique groups allowing user filter methods by particular severity level of problem.

I am based on async method query. And in the browser I am animated to indicate query background processing
"
Class {
	#name : #ClyAllCritiquesMethodGroup,
	#superclass : #ClyAsyncMethodGroup,
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #'instance creation' }
ClyAllCritiquesMethodGroup class >> withMethods: aMethodQuery from: aClassScope [
	| subgroupsQuery |
	subgroupsQuery := ClyAllMethodCritiqueQuery 
		from: aClassScope as: ClyGroupedCritiqueResult new.
	
	^self named: 'critiques' priority: 8.5 on: aMethodQuery withSubgroupsFrom: subgroupsQuery
]

{ #category : #accessing }
ClyAllCritiquesMethodGroup >> loadAsyncQueryResult [
	"Here method query is based on actual critiques query. 
	And each subgroup method query is also based on it. 
	So we need to hold reference to based critue query result instead of method query"
	^methodQuery loadCritiquesAsync
]
