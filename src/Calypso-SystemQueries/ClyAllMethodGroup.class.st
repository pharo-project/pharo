"
I includes all methods from concrete meta level of given class.

I represented in the browser either with ""instance side"" or ""class side"" names depending on concrete meta level of my method query.

I am expanded to inherited method groups for each of trait or class which are inherited by my class scope
"
Class {
	#name : #ClyAllMethodGroup,
	#superclass : #ClyInheritedMethodGroup,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'instance creation' }
ClyAllMethodGroup class >> withMethodQuery: aMethodQuery andClassesInheritedFrom: aClassScope [
	| group |
	group := super withMethodQuery: aMethodQuery andClassesInheritedFrom: aClassScope.
	group name: (aMethodQuery scope localScopeClass = ClyInstanceSideScope 
		ifTrue: [ 'instance side' ]
		ifFalse: [ 'class side' ]).
	^group
]

{ #category : #'instance creation' }
ClyAllMethodGroup class >> withMethodsInheritedFrom: aClassScope [

	^self  
		withMethodQuery: (ClyAllMethodsQuery from: aClassScope asLocalClassScope)
		andClassesInheritedFrom: aClassScope
]

{ #category : #operations }
ClyAllMethodGroup >> importMethod: aMethod [ 
	"To support some people scenario to unclassify methods 
	by drag and drop into this group (which was with ALL name in past)"
	
	aMethod tags do: [ :each | aMethod untagFrom: each ]
]
