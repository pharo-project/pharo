"
I am the ""Query"" group of the ""Source code"" sub-menu of code editors (SycSourceCodeMenuGroup): the commands browsing what the source node refers to, shown first, between lines.
"
Class {
	#name : 'SycSourceCodeQueryMenuGroup',
	#superclass : 'ClyQueryMenuGroup',
	#category : 'Calypso-SystemTools-Core-Menu groups',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Menu groups'
}

{ #category : 'parents' }
SycSourceCodeQueryMenuGroup class >> parentGroup [

	^ SycSourceCodeMenuGroup
]
