"
I filter objects by their selector using given string filter.

So I am compatible with objects which respond to #selector.
"
Class {
	#name : #ClyItemSelectorFilter,
	#superclass : #ClyItemStringFilter,
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #testing }
ClyItemSelectorFilter >> matches: anEnvironmentItem [

	^ pattern matches: anEnvironmentItem selector
]
