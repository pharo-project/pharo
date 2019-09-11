Class {
	#name : #SpAbstractWidgetAdapterTestCase,
	#superclass : #SpAbstractAdapterTestCase,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractWidgetAdapterTestCase class >> isAbstract [ 

	^ self name = #SpAbstractWidgetAdapterTestCase 
]

{ #category : #running }
SpAbstractWidgetAdapterTestCase >> testChangingHelpAffectTheWidget [
	
	presenter help: 'ALabel'.
	self assert: self adapter helpText equals: 'ALabel'
]

{ #category : #running }
SpAbstractWidgetAdapterTestCase >> testEnabledAffectWidget [
	
	presenter enabled: false.
	self deny: self adapter enabled
]
