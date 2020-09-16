"
An object inspector specialized for debuggers.
I show only a compacted raw view instead of a notebook page, in particular I remove tabs and provide an optional layout without a header.
Activating one of my items (by clicking or double-clicking on it) shows a miller list with a standard object inspector.
"
Class {
	#name : #StDebuggerRawObjectInspector,
	#superclass : #StObjectInspector,
	#instVars : [
		'raw',
		'filterAllRadio',
		'filterSelfRadio',
		'filterTempsRadio',
		'filterArgsRadio',
		'filterContextRadio',
		'sortLabel'
	],
	#category : #'NewTools-Debugger-View'
}

{ #category : #specs }
StDebuggerRawObjectInspector class >> defaultSpec [

	<spec: #default>
	^ SpBoxLayout newVertical
		add: #header withConstraints: [ :constraints |  constraints height: 22 ];
		add: (SpBoxLayout newHorizontal
				   add: #sortLabel withConstraints: [ :constraints | constraints width: 45 ];			   
					add: #filterArgsRadio withConstraints: [ :constraints | constraints width: 50 ];
				   add: #filterTempsRadio withConstraints: [ :constraints | constraints width: 60 ];
				   add: #filterSelfRadio withConstraints: [ :constraints | constraints width: 35 ];
				   add: #filterContextRadio withConstraints: [ :constraints | constraints width: 50 ];
				   yourself) withConstraints: [ :constraints |  constraints height: self toolbarHeight ];
		add: #raw;
		  yourself
]

{ #category : #specs }
StDebuggerRawObjectInspector class >> noHeaderSpec [
	<spec: #default>
	^ SpBoxLayout newVertical
		add: #raw;
		yourself
]

{ #category : #initialization }
StDebuggerRawObjectInspector >> initializeFiltersRadioButtons [
	sortLabel := self newLabel.
	sortLabel label: 'Sort by: '.
	filterSelfRadio := self newRadioButton.
	filterTempsRadio := self newRadioButton.
	filterArgsRadio := self newRadioButton.
	filterContextRadio := self newRadioButton.
	
	filterArgsRadio state: true.	
			
	filterSelfRadio label: 'self'.
	filterSelfRadio whenActivatedDo: [ self sortInspectorTable: 'attribute'].
	filterTempsRadio label: 'temps'.
	filterTempsRadio whenActivatedDo: [ self sortInspectorTable: 'temp'  ].
	filterArgsRadio label: 'args'.
	filterArgsRadio whenActivatedDo: [ self sortInspectorTable: 'arg'  ].
	filterContextRadio label: 'context'.
	filterContextRadio whenActivatedDo: [ self sortInspectorTable: 'implicit'  ].
		
	filterArgsRadio associatedRadioButtons: 
		{filterSelfRadio. filterTempsRadio. filterContextRadio}
]

{ #category : #initialization }
StDebuggerRawObjectInspector >> initializePresenters [
	super initializePresenters.
	raw := self model inspectionRaw.
	raw beForDebugger.
	raw owner: self.
	self addActivationTo: raw.
	self initializeFiltersRadioButtons
]

{ #category : #initialization }
StDebuggerRawObjectInspector >> rawInspectorPageOrNil [
	^raw
]

{ #category : #initialization }
StDebuggerRawObjectInspector >> sortInspectorTable: variableTag [
	|roots newRoots|
	roots := self rawInspectorPageOrNil attributeTable roots.
	newRoots := OrderedCollection new.
	newRoots addAll: (roots select: [:node| node variableTag = variableTag]).
	newRoots addAll: (roots reject: [:node| node variableTag = variableTag]).
	self rawInspectorPageOrNil attributeTable roots: newRoots
]

{ #category : #stepping }
StDebuggerRawObjectInspector >> step [
	super step.
	raw step
]
