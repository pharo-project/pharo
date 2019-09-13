Class {
	#name : #SpAbstractWidgetAdapterTest,
	#superclass : #SpAbstractAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractWidgetAdapterTest class >> isAbstract [ 

	^ self name = #SpAbstractWidgetAdapterTest 
]

{ #category : #running }
SpAbstractWidgetAdapterTest >> testChangingHelpAffectTheWidget [
	
	presenter help: 'ALabel'.
	self assert: self adapter helpText equals: 'ALabel'
]

{ #category : #running }
SpAbstractWidgetAdapterTest >> testEnabledAffectWidget [
	
	presenter enabled: false.
	self deny: self adapter enabled
]
