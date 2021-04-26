"
I special variable query result which filters instance variables and represents them as ClyVariableMethodGroup instances in hierarchical order.

In addition I add extra method group for class variables if they exist. It shows all class variables when it is expanded.
And I add default ClyAllMethodGroup as first item. 
"
Class {
	#name : #ClyGroupedInstanceVariablesResult,
	#superclass : #ClyGroupedVariablesResult,
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #building }
ClyGroupedInstanceVariablesResult >> addClassVariableGroup [

	| variableQuery methodQuery group |
	variableQuery := ClyAllClassVariablesQuery from: self queryScope.
	methodQuery := ClyVariableReferencesQuery 
		from: self queryScope asLocalClassScope
		toVariablesFrom: variableQuery.
	
	group := ClyMethodGroup 
		named: 'Class variables' priority: 9 
		on: methodQuery 
		withSubgroupsFrom: (variableQuery withResult: (ClyGroupedClassVariablesResult withHierarchy: hierarchy)).
	items add: group asCalypsoBrowserItem
]

{ #category : #building }
ClyGroupedInstanceVariablesResult >> addInheritedMethodGroup [

	| group |
	group := ClyAllMethodGroup withMethodsInheritedFrom: self queryScope. 
	items add: group asCalypsoBrowserItem
]

{ #category : #building }
ClyGroupedInstanceVariablesResult >> fillWithGroupedVariables: varsPerTypeAndDefiningClass [
	self addInheritedMethodGroup.
	
	varsPerTypeAndDefiningClass at: ClyClassVariable ifPresent: [:classVarsPerClass |
		self addClassVariableGroup].
	varsPerTypeAndDefiningClass at: ClyInstanceVariable ifPresent: [:instVarsPerClass |
		self fillWithVariables: instVarsPerClass type: ClyInstanceVariable]
]
