Class {
	#name : #AbstractWidgetAdapterTestCase,
	#superclass : #AbstractAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #testing }
AbstractWidgetAdapterTestCase class >> isAbstract [ 

	^ self name = #AbstractWidgetAdapterTestCase 
]

{ #category : #running }
AbstractWidgetAdapterTestCase >> testChangingHelpAffectTheWidget [
	
	presenter help: 'ALabel'.
	self assert: self adapter helpText equals: 'ALabel'
]

{ #category : #running }
AbstractWidgetAdapterTestCase >> testEnabledAffectWidget [
	
	presenter enabled: false.
	self deny: self adapter enabled
]
