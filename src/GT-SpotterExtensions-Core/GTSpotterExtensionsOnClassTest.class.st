Class {
	#name : #GTSpotterExtensionsOnClassTest,
	#superclass : #TestCase,
	#category : #'GT-SpotterExtensions-Core'
}

{ #category : #testing }
GTSpotterExtensionsOnClassTest class >> isDeprecated [
	^ true
]

{ #category : #tests }
GTSpotterExtensionsOnClassTest >> testBasic [

	| def cls |
	cls := self class.
	def := cls definitionForSpotter.
	self assert: (def beginsWith: cls definitionString)
]
