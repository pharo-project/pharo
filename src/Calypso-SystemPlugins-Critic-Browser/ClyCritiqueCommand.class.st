"
I am a root of commands hierarhcy which perform various set of operations with critiques.

My subclasses are shown as icons in critiques tables.

Internal Representation and Key Implementation Points.

    Instance Variables
	analyzedObject:		<Object>
	critique:		<Object>
"
Class {
	#name : #ClyCritiqueCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'critique',
		'analyzedObject'
	],
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #testing }
ClyCritiqueCommand class >> canBeExecutedInContext: aCriticContext [
	
	^aCriticContext hasSelectedItems and: [
		(aCriticContext lastSelectedItem isMarkedWith: ClyBackgroundProcessingTag) not]
]

{ #category : #activation }
ClyCritiqueCommand class >> criticTableIconActivation [
	<classAnnotation>
	
	^ClyTableIconCommandActivation for: ReAbstractCritique asCalypsoItemContext 
]

{ #category : #testing }
ClyCritiqueCommand class >> isAbstract [
	^self = ClyCritiqueCommand 
]

{ #category : #accessing }
ClyCritiqueCommand >> analyzedObject [
	^ analyzedObject
]

{ #category : #accessing }
ClyCritiqueCommand >> analyzedObject: anObject [
	analyzedObject := anObject
]

{ #category : #accessing }
ClyCritiqueCommand >> critique [
	^ critique
]

{ #category : #accessing }
ClyCritiqueCommand >> critique: anObject [
	critique := anObject
]

{ #category : #'table decoration' }
ClyCritiqueCommand >> decorateTableCell: anItemCellMorph with: aMorph [

	aMorph setBalloonText: self description.
	anItemCellMorph addExtraTool: aMorph
]

{ #category : #'table decoration' }
ClyCritiqueCommand >> description [
	^''
]

{ #category : #execution }
ClyCritiqueCommand >> readParametersFromContext: aCriticContext [
	super readParametersFromContext: aCriticContext.
	
	critique := aCriticContext selectedCritique.
	analyzedObject := aCriticContext analyzedObject
]
