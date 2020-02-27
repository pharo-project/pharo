"
I install shortcuts of Spec commands in my #presenter.
"
Class {
	#name : #SpShortcutInstaller,
	#superclass : #CmVisitor,
	#instVars : [
		'presenter'
	],
	#category : #'Spec2-Commander2-Visitors'
}

{ #category : #accessing }
SpShortcutInstaller >> presenter [
	^ presenter
]

{ #category : #accessing }
SpShortcutInstaller >> presenter: anObject [
	presenter := anObject
]

{ #category : #visiting }
SpShortcutInstaller >> visitCommand: aCmCommand [
	aCmCommand hasShortcutKey
		ifFalse: [ ^ self ].
	self presenter
		bindKeyCombination: aCmCommand shortcutKey
		toAction: [ aCmCommand canBeExecuted
				ifTrue: [ aCmCommand execute ] ]
]
