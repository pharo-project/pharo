"
A GLMTabulator is an explicit browser that allows us to place panes in columns and rows.
"
Class {
	#name : #GLMTabulator,
	#superclass : #GLMExplicitBrowser,
	#instVars : [
		'cell'
	],
	#category : #'Glamour-Browsers-Tabulator'
}

{ #category : #layout }
GLMTabulator >> addColumn: aBlockOrSymbol [ 
	 
	^self cellOrRow addColumn: aBlockOrSymbol
]

{ #category : #layout }
GLMTabulator >> addColumn: aBlockOrSymbol size: anInteger [ 
	 
	^(self addColumn: aBlockOrSymbol) size: anInteger; span: 0
]

{ #category : #layout }
GLMTabulator >> addColumn: aBlockOrSymbol span: anInteger [ 
	 
	^(self addColumn: aBlockOrSymbol) span: anInteger
]

{ #category : #layout }
GLMTabulator >> addRow: aBlockOrSymbol [ 
	 
	^self cellOrColumn addRow: aBlockOrSymbol
]

{ #category : #layout }
GLMTabulator >> addRow: aBlockOrSymbol size: anInteger [ 
	 
	^(self addRow: aBlockOrSymbol) size: anInteger; span: 0
]

{ #category : #layout }
GLMTabulator >> addRow: aBlockOrSymbol span: anInteger [ 
	 
	^(self addRow: aBlockOrSymbol) span: anInteger
]

{ #category : #layout }
GLMTabulator >> cell [ 
	 
	^cell
]

{ #category : #private }
GLMTabulator >> cellOrColumn [ 
	 
	^cell ifNil: [cell := GLMCustomColumn new browser: self]
]

{ #category : #private }
GLMTabulator >> cellOrRow [ 
	 
	^cell ifNil: [cell := GLMCustomRow new browser: self]
]

{ #category : #scripting }
GLMTabulator >> column: aBlockOrSymbol [ 
	 
	^self addColumn: aBlockOrSymbol
]

{ #category : #scripting }
GLMTabulator >> column: aBlockOrSymbol size: anInteger [ 
	 
	^self addColumn: aBlockOrSymbol size: anInteger
]

{ #category : #scripting }
GLMTabulator >> column: aBlockOrSymbol span: anInteger [ 
	 
	^self addColumn: aBlockOrSymbol span: anInteger
]

{ #category : #scripting }
GLMTabulator >> initializeScriptingDefaults [

	super initializeScriptingDefaults.
	self cell ifNil: [ self addColumn: #pane ].
	self transmissions
		detect: [ :each | each originatesAt: ( self pane port: #entity ) ]
		ifNone: [ self transmit
				to: ( self panes first port: #entity );
				from: #outer port: #entity;
				andShow: [ :a | a custom: GLMFlexiblePresentation new ]
			]
]

{ #category : #rendering }
GLMTabulator >> renderGlamorouslyOn: aRenderer [ 
	 
	^aRenderer renderTabulator: self
]

{ #category : #scripting }
GLMTabulator >> row: aBlockOrSymbol [ 
	 
	^self addRow: aBlockOrSymbol
]

{ #category : #scripting }
GLMTabulator >> row: aBlockOrSymbol size: anInteger [ 
	 
	^self addRow: aBlockOrSymbol size: anInteger
]

{ #category : #scripting }
GLMTabulator >> row: aBlockOrSymbol span: anInteger [ 
	 
	^self addRow: aBlockOrSymbol span: anInteger
]
