Class {
	#name : #DoNotBreakFlowOfOthersTest,
	#superclass : #TestCase,
	#category : #'Calypso-Browser-Tests'
}

{ #category : #tests }
DoNotBreakFlowOfOthersTest >> testBrowseViaImplementorActuallyBrowseTheClass [

	| browser res |
	browser := ClyFullBrowserMorph new.
	res := (browser browseImplementorsOf: #Object inNameResolver: nil).
	self assert: res second equals: #BrowseClassWithoutNameResolver.
	res first close
]
