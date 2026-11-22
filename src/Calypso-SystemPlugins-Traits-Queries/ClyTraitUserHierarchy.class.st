"
I represent hierarchy of classes where roots are common traits or classes without traits.
The children of every trait are user classes or traits.
And basic classes has no children because they are not traits
"
Class {
	#name : 'ClyTraitUserHierarchy',
	#superclass : 'ClyClassHierarchy',
	#category : 'Calypso-SystemPlugins-Traits-Queries',
	#package : 'Calypso-SystemPlugins-Traits-Queries'
}

{ #category : 'method visibility' }
ClyTraitUserHierarchy class >> hierarchyForMethodVisibility [
	"It will add inherited traits to the list of inherited classes to toggle visibility of methods"
	^(ClySubclassHierarchy hierarchyForMethodVisibility, self inverse)
		sortedBy: ClyTraitFirstSortFunction ascending
]

{ #category : 'method visibility' }
ClyTraitUserHierarchy class >> methodVisibilityProvider [
	"With traits, the method visibility hierarchy also shows the traits. See ClyMethodVisibilityGroupsResult class>>#methodVisibilityProvider."

	<calypsoMethodVisibilityProvider: 100>
]

{ #category : 'building' }
ClyTraitUserHierarchy >> buildParentMap: aHierarchyMap for: aClass [
	| allParents eachParent |
	allParents := IdentitySet withAll: aClass traits.
	[allParents isEmpty] whileFalse: [
		eachParent := allParents anyOne.
		allParents remove: eachParent.
		(aHierarchyMap includesClass: eachParent)
			ifTrue: [aHierarchyMap addChild: aClass to: eachParent]
			ifFalse: [ allParents addAll: eachParent traits].
	]
]
