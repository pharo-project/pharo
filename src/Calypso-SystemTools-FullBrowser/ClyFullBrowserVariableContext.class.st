"
I am a context of selected variables in full browser
"
Class {
	#name : #ClyFullBrowserVariableContext,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyFullBrowserVariableContext class >> selectionStrategy [
	<classAnnotation>
	
	^(ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyVariableMethodGroup)
		priority: 1000 "variable context should be with more priority than basic method group context"
]

{ #category : #'selection strategy' }
ClyFullBrowserVariableContext class >> selectionStrategy2 [
	<classAnnotation>
	
	^(ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyVariable)
]

{ #category : #'selection-variables' }
ClyFullBrowserVariableContext >> isGlobalVariableSelected [
	^false
]

{ #category : #'selection-variables' }
ClyFullBrowserVariableContext >> isVariableSelected [
	^self hasSelectedItems 
]

{ #category : #'selection-variables' }
ClyFullBrowserVariableContext >> lastSelectedVariable [
	^self lastSelectedObject variable
]

{ #category : #'selection-variables' }
ClyFullBrowserVariableContext >> selectedVariables [
	^self selectedObjects collect: [ :each | each variable ]
]
