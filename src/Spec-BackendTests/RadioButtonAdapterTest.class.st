Class {
	#name : #RadioButtonAdapterTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #running }
RadioButtonAdapterTest >> classToTest [

	^ RadioButtonPresenter 
]

{ #category : #running }
RadioButtonAdapterTest >> testChangingLabelAffectTheWidget [
	
	presenter label: 'ALabel'.
	self assert: self widget label equals: 'ALabel'
]

{ #category : #tests }
RadioButtonAdapterTest >> testIfSingleRadioButtonStartsSelected [

	self assert: self adapter state.
]
