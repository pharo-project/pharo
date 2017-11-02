Class {
	#name : #GLMCustomRow,
	#superclass : #GLMCustomCell,
	#category : 'Glamour-Browsers-Tabulator'
}

{ #category : #accessing }
GLMCustomRow >> addColumn: aBlockOrSymbol [ 
	 
	| newColumn cell | 
	cell := aBlockOrSymbol isSymbol 
		ifTrue: 
			[self browser 
				addNewPaneNamed: aBlockOrSymbol. 
			(GLMCustomColumn new) 
				id: aBlockOrSymbol; 
				browser: self browser] 
		ifFalse: 
			[newColumn := GLMCustomColumn new browser: self browser. 
			aBlockOrSymbol value: newColumn. 
			newColumn]. 
	self children addLast: cell. 
	^cell
]

{ #category : #accessing }
GLMCustomRow >> addColumn: aBlockOrSymbol size: anInteger [ 
	 
	^(self addColumn: aBlockOrSymbol) size: anInteger; span: 0
]

{ #category : #accessing }
GLMCustomRow >> addColumn: aBlockOrSymbol span: anInteger [ 
	 
	^(self addColumn: aBlockOrSymbol) span: anInteger; size: 0
]

{ #category : #scripting }
GLMCustomRow >> column: aBlockOrSymbol [ 
	 
	^self addColumn: aBlockOrSymbol
]

{ #category : #scripting }
GLMCustomRow >> column: aBlockOrSymbol size: anInteger [ 
	 
	^self addColumn: aBlockOrSymbol size: anInteger
]

{ #category : #scripting }
GLMCustomRow >> column: aBlockOrSymbol span: anInteger [ 
	 
	^self addColumn: aBlockOrSymbol span: anInteger
]

{ #category : #testing }
GLMCustomRow >> isRow [ 
	 
	^true
]
