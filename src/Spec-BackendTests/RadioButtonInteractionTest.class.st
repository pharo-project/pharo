Class {
	#name : #RadioButtonInteractionTest,
	#superclass : #AbstractAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
RadioButtonInteractionTest >> classToTest [

	^ TestPresenterWithThreeRadioButtons 
]

{ #category : #accessing }
RadioButtonInteractionTest >> testClickingRadio3DeselectsRadio1 [

	self openInstance.
	self presenter radio3 adapter clicked.
	
	self deny: self presenter radio1 state
]

{ #category : #accessing }
RadioButtonInteractionTest >> testClickingRadio3DeselectsRadio2 [

	self openInstance.
	self presenter radio3 adapter clicked.
	
	self deny: self presenter radio2 state
]

{ #category : #accessing }
RadioButtonInteractionTest >> testClickingRadio3SelectsIt [

	self openInstance.
	self presenter radio3 adapter clicked.
	
	self assert: self presenter radio3 state
]

{ #category : #accessing }
RadioButtonInteractionTest >> testHidePresenterHidesWidget [

	"Composable presenters do not have adapters so this test breaks..."
	self skip
]

{ #category : #accessing }
RadioButtonInteractionTest >> testRadio1StartsDeselected [

	self deny: self presenter radio1 state
]

{ #category : #accessing }
RadioButtonInteractionTest >> testRadio2StartsSelected [

	self assert: self presenter radio2 state
]

{ #category : #accessing }
RadioButtonInteractionTest >> testRadio2WidgetStartsSelected [

	self openInstance.

	self assert: self presenter radio2 adapter state
]

{ #category : #accessing }
RadioButtonInteractionTest >> testRadio3StartsDeselected [

	self openInstance.
	self deny: self presenter radio3 state
]

{ #category : #accessing }
RadioButtonInteractionTest >> testShowPresenterShowsWidget [

	"Composable presenters do not have adapters so this test breaks..."
	self skip
]
