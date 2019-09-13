Class {
	#name : #SpRadioButtonInteractionTest,
	#superclass : #SpAbstractAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpRadioButtonInteractionTest >> classToTest [

	^ SpTestPresenterWithThreeRadioButtons 
]

{ #category : #accessing }
SpRadioButtonInteractionTest >> testClickingRadio3DeselectsRadio1 [

	self openInstance.
	self presenter radio3 adapter clicked.
	
	self deny: self presenter radio1 state
]

{ #category : #accessing }
SpRadioButtonInteractionTest >> testClickingRadio3DeselectsRadio2 [

	self openInstance.
	self presenter radio3 adapter clicked.
	
	self deny: self presenter radio2 state
]

{ #category : #accessing }
SpRadioButtonInteractionTest >> testClickingRadio3SelectsIt [

	self openInstance.
	self presenter radio3 adapter clicked.
	
	self assert: self presenter radio3 state
]

{ #category : #accessing }
SpRadioButtonInteractionTest >> testHidePresenterHidesWidget [

	"Composable presenters do not have adapters so this test breaks..."
	self skip
]

{ #category : #accessing }
SpRadioButtonInteractionTest >> testRadio1StartsDeselected [

	self deny: self presenter radio1 state
]

{ #category : #accessing }
SpRadioButtonInteractionTest >> testRadio2StartsSelected [

	self assert: self presenter radio2 state
]

{ #category : #accessing }
SpRadioButtonInteractionTest >> testRadio2WidgetStartsSelected [

	self openInstance.

	self assert: self presenter radio2 adapter state
]

{ #category : #accessing }
SpRadioButtonInteractionTest >> testRadio3StartsDeselected [

	self openInstance.
	self deny: self presenter radio3 state
]

{ #category : #accessing }
SpRadioButtonInteractionTest >> testShowPresenterShowsWidget [

	"Composable presenters do not have adapters so this test breaks..."
	self skip
]
