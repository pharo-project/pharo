Class {
	#name : #MethodWidgetTest,
	#superclass : #TestCase,
	#category : #'Nautilus-Tests'
}

{ #category : #mock }
MethodWidgetTest >> foo [
	<script: 'self new'>
]

{ #category : #tests }
MethodWidgetTest >> testElementInCacheShouldBeNotNil [

	MethodWidget resetMethodIconCache.
	self assert: (MethodWidget new methodIconFor: (self class >> #foo)) target notNil.
	Smalltalk garbageCollect.
	self assert: (MethodWidget new methodIconFor: (self class >> #foo)) target notNil.
]
