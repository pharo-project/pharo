"
I am group of inherited methods. 
Depending on my query I include methods from on or all traits and superclasses which are inherited by given classes.

I can be expanded by subgroups of each inherited class. It will be also my instances but they will be configured by single class scope method query.

In the browser I provide few checkboxes to switch method visibility. It is based on my visibilityLevels, collection of ClyMethodVisibilityLevel instances.
They are extended by plugins. For example Traits plugin adds local trait visibility level. 


Internal Representation and Key Implementation Points.

    Instance Variables
	visibilityLevels:		<Collection of<ClyMethodVisibilityLevel>>

"
Class {
	#name : #ClyInheritedMethodGroup,
	#superclass : #ClyMethodGroup,
	#instVars : [
		'visibilityLevels'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #accessing }
ClyInheritedMethodGroup class >> priority [
	^10
]

{ #category : #'instance creation' }
ClyInheritedMethodGroup class >> withMethodQuery: aMethodQuery andClassesInheritedFrom: aClassScope [
	| subgroupsQuery |
	subgroupsQuery := ClyAllClassesQuery 
		from: aClassScope asInheritedScope, aClassScope asLocalClassScope
		as: ClyMethodVisibilityGroups withDefaultHierarchy.		
				
	^(self named: 'inherited methods' priority: 10 on: aMethodQuery withSubgroupsFrom: subgroupsQuery)
		visibilityLevels: { ClyFullVisibilityLevel of: aClassScope }
]

{ #category : #'instance creation' }
ClyInheritedMethodGroup class >> withMethodsInheritedFrom: aClassScope [

	^self  
		withMethodQuery: (ClyAllMethodsQuery from: aClassScope asInheritedScope)
		andClassesInheritedFrom: aClassScope
]

{ #category : #'instance creation' }
ClyInheritedMethodGroup class >> withMethodsOf: aClass toShowIn: aClassScope [
	
	| methodQuery |
	methodQuery := ClyAllMethodsQuery 
		from: (aClassScope asScope: ClyClassScope of: aClass).
				
	^(self named: aClass name on: methodQuery)
		visibilityLevels: { ClySuperclassVisibilityLevel of: aClassScope toShow: aClass }
]

{ #category : #testing }
ClyInheritedMethodGroup >> includesMethod: aMethod [
	"Original version:
		^self methodQuery retrievesItem: aMethod
	shows bad logic behind #retrievesItem: method. It do not take into account scope of the query.
	It can be addressed in future. 
	Now we just think that argument here could not be inherited method. So the group never include it"
	
	^ true
]

{ #category : #operations }
ClyInheritedMethodGroup >> removeWithMethods [
	self error: 'You should not remove inherited methods'
]

{ #category : #accessing }
ClyInheritedMethodGroup >> visibilityLevels [
	^ visibilityLevels
]

{ #category : #accessing }
ClyInheritedMethodGroup >> visibilityLevels: levelCollection [
	visibilityLevels := levelCollection reject: [ :each | each isRedundant ]
]
