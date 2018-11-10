"
I'm a filter for repositories, to be used in a repository list.

currently I'm used to add filters in IceTipRepositoriesBrowser.
"
Class {
	#name : #IceTipRepositoryFilter,
	#superclass : #IceTipFilter,
	#category : #'Iceberg-TipUI-View'
}

{ #category : #testing }
IceTipRepositoryFilter >> matches: aRepositoryModel [
	^  aRepositoryModel name asLowercase includesSubstring: self pattern asLowercase
]
