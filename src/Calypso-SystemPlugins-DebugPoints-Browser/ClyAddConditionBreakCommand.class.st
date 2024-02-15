"
I define a command to add a conditional breakpoint on the AST node corresponding to the selected code
"
Class {
	#name : 'ClyAddConditionBreakCommand',
	#superclass : 'ClyDebuggingPointsCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'testing' }
ClyAddConditionBreakCommand class >> canBeExecutedInContext: aBrowserContext [

	^ (super canBeExecutedInContext: aBrowserContext)
]

{ #category : 'activation' }
ClyAddConditionBreakCommand class >> contextMenuOrder [
	^3
]

{ #category : 'accessing' }
ClyAddConditionBreakCommand >> defaultMenuIconName [
	^#smallError
]

{ #category : 'accessing' }
ClyAddConditionBreakCommand >> defaultMenuItemName [
	^' Add Condition Breakpoint to: ',  sourceNode displaySourceCode
]

{ #category : 'execution' }
ClyAddConditionBreakCommand >> execute [

	| dp |
	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      On: sourceNode
		      withBehaviors: { ConditionBehavior }.
 
	dp condition: self popUp.
	self putIconInProperties: dp
]

{ #category : 'ui - dialogs' }
ClyAddConditionBreakCommand >> popUp [
	|textInput|
	textInput := SpRequestDialog new
		label: 'Enter condition:';
		text: '';
		title: 'Condition';
		openModal.
	textInput ifNil: [ ^'' ].
	^textInput 
]
