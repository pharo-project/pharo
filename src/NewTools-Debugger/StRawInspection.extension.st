Extension { #name : #StRawInspection }

{ #category : #'*NewTools-Debugger' }
StRawInspection >> activePresenter [
	^self
]

{ #category : #'*NewTools-Debugger' }
StRawInspection >> addVariableTagColumn [

	attributeTable addColumn: self variableTagColumn
]

{ #category : #'*NewTools-Debugger' }
StRawInspection >> attributeTable [

	^attributeTable
]

{ #category : #'*NewTools-Debugger' }
StRawInspection >> beForDebugger [

	attributeTable := (self instantiate: StDebuggerTreeTablePresenter)
		beResizable;
		contextMenu: [ self rootCommandsGroup asMenuPresenter ];
		yourself.
		
	StPharoSettings inspectorDiveOnSingleClick 
		ifTrue: [ attributeTable activateOnSingleClick ].
	
	self addVariableTagColumn.
	self addVariablesColumn.
	self addValuesColumn.	

	attributeTable
		roots: self inspectorNodes;
		children: [ :node | #() ]
]

{ #category : #'*NewTools-Debugger' }
StRawInspection >> selectedPageName [

	^'Context'
]

{ #category : #'*NewTools-Debugger' }
StRawInspection >> variableTagColumn [

	^  SpLinkTableColumn new 
		title: 'Type';
		width: 70;
		evaluated: [ :aNode | aNode variableTag ];
		action: [ :aNode |  attributeTable toggleFilter: aNode variableTag. owner updateHeaderBar. ];
		sortFunction: #variableTag ascending;
		yourself
]
