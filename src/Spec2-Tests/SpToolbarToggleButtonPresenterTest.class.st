"
A SpToolBarToggleButtonTest is a test class for testing the behavior of SpToolBarToggleButton
"
Class {
	#name : #SpToolbarToggleButtonPresenterTest,
	#superclass : #TestCase,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #tests }
SpToolbarToggleButtonPresenterTest >> testBecomeSelectedWhenToggledAndUnselected [
	| toggleButton |
	toggleButton := SpToolbarToggleButtonPresenter new.
	toggleButton beSelected.
	self assert: toggleButton isSelected.
	
	toggleButton toggle.
	
	self deny: toggleButton isSelected.
]

{ #category : #tests }
SpToolbarToggleButtonPresenterTest >> testBecomeUnselectedWhenToggledAndSelected [
	| toggleButton |
	toggleButton := SpToolbarToggleButtonPresenter new.
	self deny: toggleButton isSelected.
		
	toggleButton toggle.
	
	self assert: toggleButton isSelected.

]

{ #category : #tests }
SpToolbarToggleButtonPresenterTest >> testSelectedBlockExecutedWhenBecomeSelected [
	| toggleButton selectedBlockExecuted |
	selectedBlockExecuted := false.
	toggleButton := SpToolbarToggleButtonPresenter new.
	toggleButton whenSelectedDo: [ selectedBlockExecuted := true ].
	
	toggleButton beSelected.
		
	self assert: selectedBlockExecuted
]

{ #category : #tests }
SpToolbarToggleButtonPresenterTest >> testSelectedBlockNotExecutedWhenAlreadySelectedAndUnselectedTriggered [
	| toggleButton selectedBlockExecuted |
	selectedBlockExecuted := false.
	toggleButton := SpToolbarToggleButtonPresenter new.
	toggleButton beSelected.
	toggleButton whenSelectedDo: [ selectedBlockExecuted := true ].
	
	toggleButton beSelected.
		
	self deny: selectedBlockExecuted
]

{ #category : #tests }
SpToolbarToggleButtonPresenterTest >> testSelectedBlockNotExecutedWhenBecomeUnselected [
	| toggleButton selectedBlockExecuted |
	selectedBlockExecuted := false.
	toggleButton := SpToolbarToggleButtonPresenter new.
	toggleButton beSelected.
	toggleButton whenSelectedDo: [ selectedBlockExecuted := true ].

	toggleButton beUnselected.	
		
	self deny: selectedBlockExecuted
]

{ #category : #tests }
SpToolbarToggleButtonPresenterTest >> testToggleBlockExecutedWhenToggled [
	| toggleButton toggled |
	toggled := false.
	toggleButton := SpToolbarToggleButtonPresenter new.
	toggleButton whenToggledDo: [ toggled := true ].
	
	toggleButton toggle.
	
	self assert: toggled
]

{ #category : #tests }
SpToolbarToggleButtonPresenterTest >> testUnselectedBlockExecutedWhenBecomeUnselected [
	| toggleButton unselectedBlockExecuted |
	unselectedBlockExecuted := false.
	toggleButton := SpToolbarToggleButtonPresenter new.
	toggleButton beSelected.
	toggleButton whenUnselectedDo: [ unselectedBlockExecuted := true ].
	
	toggleButton beUnselected.
		
	self assert: unselectedBlockExecuted
]

{ #category : #tests }
SpToolbarToggleButtonPresenterTest >> testUnselectedBlockNotExecutedWhenAlreadyUnselectedAndUnselectedTriggered [
	| toggleButton unselectedBlockExecuted |
	unselectedBlockExecuted := false.
	toggleButton := SpToolbarToggleButtonPresenter new.
	toggleButton beUnselected.
	toggleButton whenUnselectedDo: [ unselectedBlockExecuted := true ].
	
	toggleButton beUnselected.
		
	self deny: unselectedBlockExecuted
]

{ #category : #tests }
SpToolbarToggleButtonPresenterTest >> testUnselectedBlockNotExecutedWhenBecomeSelected [
	| toggleButton unselectedBlockExecuted |
	unselectedBlockExecuted := false.
	toggleButton := SpToolbarToggleButtonPresenter new.
	toggleButton beUnselected.
	toggleButton whenUnselectedDo: [ unselectedBlockExecuted := true ].
	
	toggleButton beSelected.
		
	self deny: unselectedBlockExecuted
]
