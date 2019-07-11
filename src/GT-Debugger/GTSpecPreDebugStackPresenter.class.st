"
I am a pre debugger showing to the user the beginning of a stack trace. 

I'll typically display errors (vs notifications).
 	
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	stackPresenter:		<aTablePresenter>	A table to display informations on a stack.
"
Class {
	#name : #GTSpecPreDebugStackPresenter,
	#superclass : #GTSpecPreDebugPresenter,
	#instVars : [
		'stackPresenter'
	],
	#category : #'GT-Debugger-UI'
}

{ #category : #specs }
GTSpecPreDebugStackPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #actions withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		add: #stackPresenter;
		yourself
]

{ #category : #initialization }
GTSpecPreDebugStackPresenter >> initializePresenter [
	stackPresenter
		items: self debugger filteredStack;
		whenSelectionChangedDo: [ :selection | 
			"Set the selection before, as debugAction removes the link with the debugger. "
			self debugger stackPresentation selection: selection selectedItem.
			self openFullDebugger ]
]

{ #category : #initialization }
GTSpecPreDebugStackPresenter >> initializeWidgets [
	super initializeWidgets.
	stackPresenter := self newTable.

	self flag: #todo. "We should remove the #asTextMorph but for now there is no way in Spec to use Text as table cell content."
	stackPresenter
		addColumn:
			(SpStringTableColumn
				title: 'Class'
				evaluated: [ :context | 
					(self debugger formatStackClassColumnForContext: context) asTextMorph
						lock;
						yourself ]);
		addColumn:
			(SpStringTableColumn
				title: 'Method'
				evaluated: [ :context | 
					(self debugger formatStackMethodColumnForContext: context) asTextMorph
						lock;
						yourself ]);
		addColumn:
			(SpStringTableColumn
				title: 'Extra'
				evaluated: [ :context | 
					(self debugger formatStackExtraColumnForContext: context) asTextMorph
						lock;
						yourself ]);
		beResizable.

	self focusOrder add: stackPresenter
]
