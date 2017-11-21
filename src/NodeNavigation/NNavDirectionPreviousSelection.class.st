"
I know how to go to the previous selection in the text
"
Class {
	#name : #NNavDirectionPreviousSelection,
	#superclass : #NNavDirection,
	#category : #NodeNavigation-Core
}

{ #category : #builder }
NNavDirectionPreviousSelection class >> buildShortcut: aBuilder [
	<keymap>

	(aBuilder shortcut: #previousSelection)
		category: #SmalltalkEditor
		default: $b meta shift 
		do: [ :morph | morph standOutPreviousSelection ]
		description: 'Select the previous selected scope'
]

{ #category : #'stand out' }
NNavDirectionPreviousSelection class >> findSelectionFor: navigationContext [
	^ navigationContext lastSelection 
]
