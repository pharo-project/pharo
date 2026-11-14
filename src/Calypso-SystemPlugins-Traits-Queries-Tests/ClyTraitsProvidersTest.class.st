"
Tests that the traits plugin provides the inherited scope and the method visibility hierarchy, over the defaults of Calypso, with the priorities of its <calypsoInheritedScopeProvider:> and <calypsoMethodVisibilityProvider:> pragmas.
"
Class {
	#name : 'ClyTraitsProvidersTest',
	#superclass : 'TestCase',
	#category : 'Calypso-SystemPlugins-Traits-Queries-Tests',
	#package : 'Calypso-SystemPlugins-Traits-Queries-Tests'
}

{ #category : 'tests' }
ClyTraitsProvidersTest >> testInheritedScopeIsNamedParents [

	self assert: (ClyClassScope of: OrderedCollection) asInheritedScope name equals: ClyClassScope inheritedScopeName.
	self assert: (ClyClassScope of: OrderedCollection) asInheritingScope name equals: ClyClassScope inheritingScopeName
]

{ #category : 'tests' }
ClyTraitsProvidersTest >> testTraitsProvideTheInheritedScope [

	self assert: ClyAbstractClassScope inheritedScopeProvider identicalTo: ClyInheritedTraitScope
]

{ #category : 'tests' }
ClyTraitsProvidersTest >> testTraitsProvideTheMethodVisibilityHierarchy [

	self assert: ClyMethodVisibilityGroupsResult methodVisibilityProvider identicalTo: ClyTraitUserHierarchy
]
