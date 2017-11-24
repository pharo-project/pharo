"
Monticello selector for changes
"
Class {
	#name : #MCChangeSelector,
	#superclass : #MCPatchBrowser,
	#instVars : [
		'kept'
	],
	#category : #MonticelloGUI
}

{ #category : #'morphic ui' }
MCChangeSelector >> buttonSpecs [
	^ #((Select select 'Select these changes')
		 (Cancel cancel 'Cancel the operation')
		)
]

{ #category : #actions }
MCChangeSelector >> cancel [
	self answer: nil
]

{ #category : #'morphic ui' }
MCChangeSelector >> defaultLabel [
	^ 'Change Selector'
]

{ #category : #accessing }
MCChangeSelector >> kept [
	^ kept ifNil: [kept := Set new]
]

{ #category : #'multi-selection' }
MCChangeSelector >> listSelectionAt: aNumber [
	^ self kept includes: (self items at: aNumber)
]

{ #category : #'multi-selection' }
MCChangeSelector >> listSelectionAt: aNumber put: aBoolean [
	| item |
	item := self items at: aNumber.
	aBoolean
		ifTrue: [self kept add: item ]
		ifFalse: [self kept remove: item ifAbsent: []]
]

{ #category : #emulating }
MCChangeSelector >> select [
	self answer: (MCPatch operations: kept)
]

{ #category : #selection }
MCChangeSelector >> selectAll [
	kept addAll: self items.
	self changed: #list
]

{ #category : #selection }
MCChangeSelector >> selectNone [
	kept := Set new.
	self changed: #list
]

{ #category : #'morphic ui' }
MCChangeSelector >> widgetSpecs [
	 ^#(
		((buttonRow) (0 0 1 0) (0 0 0 30))
		((multiListMorph:selection:listSelection:menu: list selection listSelectionAt: methodListMenu:) (0 0 1 0.4) (0 30 0 0))
		((buttonRow: #(('Select All' selectAll 'select all changes') ('Select None' selectNone 'select no changes'))) (0 0.4 1 0.4) (0 0 0 30))
		((textMorph: text) (0 0.4 1 1) (0 30 0 0))
		)

	
]
