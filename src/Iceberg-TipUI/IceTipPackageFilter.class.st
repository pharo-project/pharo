"
I'm a filter for packages, to be used in a package list.

currently I'm used to add filters in IceTipWorkingCopyBrowser.
"
Class {
	#name : #IceTipPackageFilter,
	#superclass : #IceTipFilter,
	#category : #'Iceberg-TipUI-View'
}

{ #category : #testing }
IceTipPackageFilter >> matches: aPackageModel [
	^ aPackageModel name asLowercase includesSubstring: self pattern asLowercase
]
