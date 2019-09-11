"
I filter objects by name using given string filter.

So I am compatible with objects which respond to the #name
"
Class {
	#name : #ClyItemNameFilter,
	#superclass : #ClyItemStringFilter,
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #testing }
ClyItemNameFilter >> matches: anEnvironmentItem [

	^pattern matches: anEnvironmentItem name
]
