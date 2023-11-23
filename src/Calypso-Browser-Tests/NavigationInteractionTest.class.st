Class {
	#name : 'NavigationInteractionTest',
	#superclass : 'TestCase',
	#category : 'Calypso-Browser-Tests',
	#package : 'Calypso-Browser-Tests'
}

{ #category : 'tests' }
NavigationInteractionTest >> testBrowseClassViaImplementorActuallyBrowseTheClass [

	| browser res |
	browser := ClyFullBrowserMorph new.
	res := (browser browseImplementorsOf: #Object inNameResolver: nil).
	self assert: res second equals: #BrowseClassWithoutNameResolver.
	res first close
]

{ #category : 'tests' }
NavigationInteractionTest >> testBrowseMethodViaImplementorActuallyBrowseTheClass [

	| browser res |
	self skip.
	"This is not possible to simply write a test for the normal case... how sad!!!"
	browser := ClyFullBrowserMorph new.
	res := (browser browseImplementorsOf: #add: inNameResolver: Object).
	"this does not work with inNameResolver: nil"
	self assert: res second equals: #NormalImplementorBrowse.
	res first close
]

{ #category : 'tests' }
NavigationInteractionTest >> testSenderOnClassViaSenderActuallyBrowseSenders [

	| browser res |
	browser := ClyFullBrowserMorph new.
	res := (browser browseImplementorsOf: #Object inNameResolver: nil).
	self assert: res second equals: #BrowseClassWithoutNameResolver.
	res first close
]
