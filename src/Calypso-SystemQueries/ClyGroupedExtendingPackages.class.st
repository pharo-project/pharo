"
I am a package query result which converts all packages to the instances of ClyExternalPackageMethodGroup.

I am used in full browser logic to show extending method groups in the third pane.
"
Class {
	#name : #ClyGroupedExtendingPackages,
	#superclass : #ClyQueryResult,
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #initialization }
ClyGroupedExtendingPackages >> defaultItems [
	^SortedCollection sortBlock: ClySortByNameFunction ascending
]

{ #category : #building }
ClyGroupedExtendingPackages >> fillWith: packages [
	| group |
	packages do: [ :each | 
		group := ClyExternalPackageMethodGroup 
			withMethodsFrom: self scope packagedIn: each.
		items add: group]
]
