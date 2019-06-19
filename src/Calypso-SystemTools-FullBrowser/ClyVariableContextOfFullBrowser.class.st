"
I am a context of selected variables in full browser
"
Class {
	#name : #ClyVariableContextOfFullBrowser,
	#superclass : #ClyFullBrowserContext,
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #'selection strategy' }
ClyVariableContextOfFullBrowser class >> selectionStrategy [
	<classAnnotation>
	
	^(ClyContextSelectionStrategy for: ClyFullBrowser selectionType: ClyVariableMethodGroup)
		priority: 1000 "variable context should be with more priority than basic method group context"
]

{ #category : #'selection strategy' }
ClyVariableContextOfFullBrowser class >> selectionStrategy2 [
	<classAnnotation>
	
	^(ClyContextSelectionStrategy for: ClyFullBrowser selectionType: ClyVariable)
]

{ #category : #'selection-variables' }
ClyVariableContextOfFullBrowser >> isGlobalVariableSelected [
	^false
]

{ #category : #'selection-variables' }
ClyVariableContextOfFullBrowser >> isVariableSelected [
	^self hasSelectedItems 
]

{ #category : #'selection-variables' }
ClyVariableContextOfFullBrowser >> lastSelectedVariable [
	^self lastSelectedObject variable
]

{ #category : #'selection-variables' }
ClyVariableContextOfFullBrowser >> selectedVariables [
	^self selectedObjects collect: [ :each | each variable ]
]
