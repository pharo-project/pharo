"
I am a package query result which converts all packages to the instances of ClyExternalPackageMethodGroup.

I am used in full browser logic to show extending method groups in the third pane.
"
Class {
	#name : #ClyGroupedExtendingPackagesResult,
	#superclass : #ClyQueryResult,
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #initialization }
ClyGroupedExtendingPackagesResult >> defaultItems [
	^SortedCollection sortBlock: ClySortByNameFunction ascending
]

{ #category : #building }
ClyGroupedExtendingPackagesResult >> fillWith: packages [
	| group |
	packages do: [ :each | 
		group := ClyExternalPackageMethodGroup 
			withMethodsFrom: self scope packagedIn: each.
		items add: group]
]
