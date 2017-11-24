Class {
	#name : #GLMCustomColumn,
	#superclass : #GLMCustomCell,
	#category : #'Glamour-Browsers-Tabulator'
}

{ #category : #accessing }
GLMCustomColumn >> addRow: aBlockOrSymbol [ 
	 
	| newRow cell | 
	cell := aBlockOrSymbol isSymbol 
		ifTrue: 
			[self browser 
				addNewPaneNamed: aBlockOrSymbol. 
			(GLMCustomRow new) 
				id: aBlockOrSymbol; 
				browser: self browser] 
		ifFalse: 
			[newRow := GLMCustomRow new browser: self browser. 
			aBlockOrSymbol value: newRow. 
			newRow]. 
	self children addLast: cell. 
	^cell
]

{ #category : #accessing }
GLMCustomColumn >> addRow: aBlockOrSymbol size: anInteger [ 
	 
	^(self addRow: aBlockOrSymbol) size: anInteger; span: 0
]

{ #category : #accessing }
GLMCustomColumn >> addRow: aBlockOrSymbol span: anInteger [ 
	 
	^(self addRow: aBlockOrSymbol) span: anInteger; size: 0
]

{ #category : #testing }
GLMCustomColumn >> isColumn [ 
	 
	^true
]

{ #category : #scripting }
GLMCustomColumn >> row: aBlockOrSymbol [ 
	 
	^self addRow: aBlockOrSymbol
]

{ #category : #scripting }
GLMCustomColumn >> row: aBlockOrSymbol size: anInteger [ 
	 
	^self addRow: aBlockOrSymbol size: anInteger
]

{ #category : #scripting }
GLMCustomColumn >> row: aBlockOrSymbol span: anInteger [ 
	 
	^self addRow: aBlockOrSymbol span: anInteger
]
