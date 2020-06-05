"
A SpToolBarToggleButtonTest is a test class for testing the behavior of SpToolBarToggleButton
"
Class {
	#name : #SpToolBarToggleButtonPresenterTest,
	#superclass : #TestCase,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #tests }
SpToolBarToggleButtonPresenterTest >> testBecomeSelectedWhenToggledAndUnselected [
	| toggleButton |
	toggleButton := SpToolBarToggleButtonPresenter new.
	toggleButton beSelected.
	self assert: toggleButton isSelected.
	
	toggleButton toggle.
	
	self deny: toggleButton isSelected.
]

{ #category : #tests }
SpToolBarToggleButtonPresenterTest >> testBecomeUnselectedWhenToggledAndSelected [
	| toggleButton |
	toggleButton := SpToolBarToggleButtonPresenter new.
	self deny: toggleButton isSelected.
		
	toggleButton toggle.
	
	self assert: toggleButton isSelected.

]

{ #category : #tests }
SpToolBarToggleButtonPresenterTest >> testSelectedBlockExecutedWhenBecomeSelected [
	| toggleButton selectedBlockExecuted |
	selectedBlockExecuted := false.
	toggleButton := SpToolBarToggleButtonPresenter new.
	toggleButton whenSelectedDo: [ selectedBlockExecuted := true ].
	
	toggleButton beSelected.
		
	self assert: selectedBlockExecuted
]

{ #category : #tests }
SpToolBarToggleButtonPresenterTest >> testSelectedBlockNotExecutedWhenAlreadySelectedAndUnselectedTriggered [
	| toggleButton selectedBlockExecuted |
	selectedBlockExecuted := false.
	toggleButton := SpToolBarToggleButtonPresenter new.
	toggleButton beSelected.
	toggleButton whenSelectedDo: [ selectedBlockExecuted := true ].
	
	toggleButton beSelected.
		
	self deny: selectedBlockExecuted
]

{ #category : #tests }
SpToolBarToggleButtonPresenterTest >> testSelectedBlockNotExecutedWhenBecomeUnselected [
	| toggleButton selectedBlockExecuted |
	selectedBlockExecuted := false.
	toggleButton := SpToolBarToggleButtonPresenter new.
	toggleButton beSelected.
	toggleButton whenSelectedDo: [ selectedBlockExecuted := true ].

	toggleButton beUnselected.	
		
	self deny: selectedBlockExecuted
]

{ #category : #tests }
SpToolBarToggleButtonPresenterTest >> testToggleBlockExecutedWhenToggled [
	| toggleButton toggled |
	toggled := false.
	toggleButton := SpToolBarToggleButtonPresenter new.
	toggleButton whenToggledDo: [ toggled := true ].
	
	toggleButton toggle.
	
	self assert: toggled
]

{ #category : #tests }
SpToolBarToggleButtonPresenterTest >> testUnselectedBlockExecutedWhenBecomeUnselected [
	| toggleButton unselectedBlockExecuted |
	unselectedBlockExecuted := false.
	toggleButton := SpToolBarToggleButtonPresenter new.
	toggleButton beSelected.
	toggleButton whenUnselectedDo: [ unselectedBlockExecuted := true ].
	
	toggleButton beUnselected.
		
	self assert: unselectedBlockExecuted
]

{ #category : #tests }
SpToolBarToggleButtonPresenterTest >> testUnselectedBlockNotExecutedWhenAlreadyUnselectedAndUnselectedTriggered [
	| toggleButton unselectedBlockExecuted |
	unselectedBlockExecuted := false.
	toggleButton := SpToolBarToggleButtonPresenter new.
	toggleButton beUnselected.
	toggleButton whenUnselectedDo: [ unselectedBlockExecuted := true ].
	
	toggleButton beUnselected.
		
	self deny: unselectedBlockExecuted
]

{ #category : #tests }
SpToolBarToggleButtonPresenterTest >> testUnselectedBlockNotExecutedWhenBecomeSelected [
	| toggleButton unselectedBlockExecuted |
	unselectedBlockExecuted := false.
	toggleButton := SpToolBarToggleButtonPresenter new.
	toggleButton beUnselected.
	toggleButton whenUnselectedDo: [ unselectedBlockExecuted := true ].
	
	toggleButton beSelected.
		
	self deny: unselectedBlockExecuted
]
