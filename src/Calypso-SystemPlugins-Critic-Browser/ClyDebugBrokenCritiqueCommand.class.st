"
I am a command to debug broken critique.
I expect that my critique is ReExceptionProperty instance representign the failure which was raisen during critique computation
"
Class {
	#name : 'ClyDebugBrokenCritiqueCommand',
	#superclass : 'ClyCritiqueCommand',
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'testing' }
ClyDebugBrokenCritiqueCommand class >> canBeExecutedInContext: aCriticContext [

	^aCriticContext hasSelectedItems and: [
		aCriticContext lastSelectedItem isMarkedWith: ClyBrokenCritiqueTag]
]

{ #category : 'activation' }
ClyDebugBrokenCritiqueCommand class >> criticTableIconActivation [
	<classAnnotation>

	^ClyTableIconCommandActivation for: ReProperty asCalypsoItemContext
]

{ #category : 'table decoration' }
ClyDebugBrokenCritiqueCommand >> decorateTableCell: anItemCellMorph with: aMorph [
	super decorateTableCell: anItemCellMorph with: aMorph.

	anItemCellMorph definitionIcon: #exception
]

{ #category : 'execution' }
ClyDebugBrokenCritiqueCommand >> executeRefactoring [

	(OupsDebugRequest newForContext: self critique stack)
		label: self critique message;
		submit
]

{ #category : 'accessing' }
ClyDebugBrokenCritiqueCommand >> iconName [
	^#smallDebug
]

{ #category : 'accessing' }
ClyDebugBrokenCritiqueCommand >> name [
	^'Debug the exception'
]
