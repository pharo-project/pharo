Class {
	#name : #GTSpotterExtensionsOnClassTest,
	#superclass : #TestCase,
	#category : #'GT-SpotterExtensions-Core'
}

{ #category : #tests }
GTSpotterExtensionsOnClassTest >> testBasic [

	| def cls |
	cls := self class.
	def := cls definitionForSpotter.
	self assert: (def beginsWith: cls definition)
]
