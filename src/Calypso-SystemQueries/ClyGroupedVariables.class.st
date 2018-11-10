"
I am abstract variable query result which represents variables as ClyVariableMethodGroup instances.
My subclases are used in full browser to show variables in third pane.

For subclasses I provide dictionary kind of variable type -> defining class -> variables where 
- variable type is class of given variable. For example there are ClyInstanceVariable and ClyClassVariable.
- defining class is a class which defines given variables 
- and variables is the sorted by name list  

So subclasses should implement method 

- fillWithGroupedVariables: varsPerTypeAndDefiningClass 

which will build required variable groups from given variable structure.
General logic to convert concrete type of variables to groups is common for all subclasses. So they just call my methods: 

- fillWithVariables: varsPerClass type: varTypeClass

which really creates variable group items.
I order variables according to their class hierarchy. The concrete kind of hierarchy is specified in my variable #hierarchy.
So you can get variable list in the order from superclass to subclass or otherwise.

To create my instances use following method: 

	ClyGroupedInstanceVariables withHierarchy: ClySubclassHierarhy new.

Or ask for default hierarchy: 

	ClyGroupedInstanceVariables withDefaultHierarchy.
	
The default hierarchy for variables are managed by ClySubclassHierarchy class:
 	
	ClySubclassHierarchy hierarchyForVariables.
	 
Internal Representation and Key Implementation Points.

    Instance Variables
	hierarchy:		<ClyClassHierarchy>
"
Class {
	#name : #ClyGroupedVariables,
	#superclass : #ClyBrowserQueryResult,
	#instVars : [
		'hierarchy'
	],
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #'instance creation' }
ClyGroupedVariables class >> defaultHierarchy [

	^ClySubclassHierarchy hierarchyForVariables 
]

{ #category : #testing }
ClyGroupedVariables class >> isExpandedHierarchy [
	^true
]

{ #category : #'instance creation' }
ClyGroupedVariables class >> withDefaultHierarchy [

	^self withHierarchy: self defaultHierarchy 
]

{ #category : #'instance creation' }
ClyGroupedVariables class >> withHierarchy: aClassHierarchy [

	^self new 
		hierarchy: aClassHierarchy
]

{ #category : #comparing }
ClyGroupedVariables >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^false ].
	
	^ hierarchy = anObject hierarchy
]

{ #category : #building }
ClyGroupedVariables >> fillWith: variables [
	| typedGroups |
	typedGroups := self groupVariablesByTypeAndDefinition: variables.
	
	self fillWithGroupedVariables: typedGroups 
]

{ #category : #building }
ClyGroupedVariables >> fillWithGroupedVariables: varsPerTypeAndDefiningClass [
	self subclassResponsibility 
]

{ #category : #building }
ClyGroupedVariables >> fillWithVariables: varsPerClass type: varTypeClass [

	| classMap vars declarationGroup varItem methodQueryScope |
	methodQueryScope := self queryScope asLocalClassScope.
	classMap := hierarchy buildFrom: varsPerClass keys.
	classMap doInOrder: [ :eachClass |
		vars := varsPerClass at: eachClass.
		declarationGroup := ClyVarDefinitionMethodGroup
			withMethodsFrom: methodQueryScope 
			referencedAny: varTypeClass definedIn: eachClass.
		items add: declarationGroup asCalypsoBrowserItem.
		vars do: [:each | | varGroup |
			varGroup := ClyVariableMethodGroup withMethodsFrom: methodQueryScope referencedVar: each.
			varItem := varGroup asCalypsoBrowserItem.
			varItem depth: 1.
			items add: varItem] 
	]
]

{ #category : #building }
ClyGroupedVariables >> groupVariablesByTypeAndDefinition: variables [

	| groups varsPerClass typedVars |
	groups := IdentityDictionary new.
	variables do: [ :var | 
		typedVars := groups at: var class ifAbsentPut: [ IdentityDictionary new ].
		varsPerClass := typedVars 
			at: var definingClass ifAbsentPut: [ self containerForItemsSortedByName ].
		varsPerClass add: var].	
	^groups
]

{ #category : #comparing }
ClyGroupedVariables >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: hierarchy hash
]

{ #category : #accessing }
ClyGroupedVariables >> hierarchy [
	^ hierarchy
]

{ #category : #accessing }
ClyGroupedVariables >> hierarchy: anObject [
	hierarchy := anObject
]

{ #category : #building }
ClyGroupedVariables >> queryScope [
	^buildingQuery scope
]
