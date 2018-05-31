Class {
	#name : #GTSpotterExtensionsOnClassTest,
	#superclass : #TestCase,
	#category : #'GT-SpotterExtensions-Core'
}

{ #category : #tests }
GTSpotterExtensionsOnClassTest >> testBasic [

	| def cls |
	cls := GTSpotterExtensionsOnClassTest.
	def := cls definitionForSpotter.
	self assert: (def beginsWith: cls definition).
	self deny: cls hasComment.
]
