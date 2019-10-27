"
I extend the simple Rubric text presentation with basic support for dealing with Smalltalk code.
"
Class {
	#name : #GLMRubricSmalltalkCodePresentation,
	#superclass : #GLMRubricTextPresentation,
	#instVars : [
		'highlightSmalltalkContext',
		'variableBindingsBlock'
	],
	#category : #'Glamour-Rubric-Presentations'
}

{ #category : #accessing }
GLMRubricSmalltalkCodePresentation >> browseFullClass [

 	self announce: (GLMTextEditorRequest for: #browseFullClass)
]

{ #category : #accessing }
GLMRubricSmalltalkCodePresentation >> browsingSelectionActions [

	^ { (GLMGenericAction new) 
				title: 'Browse Full Class';
				action: [ :aPresentation |
					aPresentation browseFullClass  ]; 
				shortcut: $b; 
				category: 'Code search...';
				yourself.
		(GLMGenericAction new) 
				title: 'Senders';
				action: [ :aPresentation |
					aPresentation sendersOfIt ]; 
				shortcut: $n; 
				category: 'Code search...';
				yourself.
		(GLMGenericAction new) 
				title: 'Implementors';
				action: [ :aPresentation |
					aPresentation implementorsOfIt ]; 
				shortcut: $m; 
				category: 'Code search...';
				yourself.
		(GLMGenericAction new) 
				title: 'References';
				action: [ :aPresentation |
					aPresentation referencesToIt ]; 
				"shortcut: $N; "
				category: 'Code search...';
				withSeparatorAfter;
				yourself.
		(GLMGenericAction new) 
				title: 'Extended string search'; 
				action: [ :aPresentation |
					aPresentation selectLine.
					self systemNavigation 
						browseMethodsWithString: aPresentation selectedText
						matchCase: false ]; 
				shortcut: $E;
				category: 'Code search...';
				yourself.
		(GLMGenericAction new) 
				title: 'Method source containing selection';
				action: [ :aPresentation |
					aPresentation selectLine.
					self systemNavigation 
						browseMethodsWithSourceString: aPresentation selectedText
						matchCase: false.]; 
				category: 'Code search...';
				withSeparatorAfter;
				yourself. }
]

{ #category : #actions }
GLMRubricSmalltalkCodePresentation >> compileTextIn: aContext from:  aSession andDo: aBlock [

	self announce: (GLMCompileText new
		presentation: self;
		context: aContext;
		session: aSession;
		action: aBlock)
]

{ #category : #actions }
GLMRubricSmalltalkCodePresentation >> debugSelection [

	self announce: GLMDebugSelection
]

{ #category : #accessing }
GLMRubricSmalltalkCodePresentation >> defaultSelectionActions [

	^ self executionSelectionActions, 
		self browsingSelectionActions,
		self codeEditingSelectionActions
]

{ #category : #actions }
GLMRubricSmalltalkCodePresentation >> evaluateSelection [

	self announce: (GLMEvaluateSelection new 
		action: [ :result | result ];
		makeSyncronous)
]

{ #category : #actions }
GLMRubricSmalltalkCodePresentation >> evaluateSelectionAndDo: aBlock [

	self announce: (GLMEvaluateSelection new action: aBlock)
]

{ #category : #accessing }
GLMRubricSmalltalkCodePresentation >> executionSelectionActions [

	^ { 
		GLMGenericAction new 
				title: 'Do it';
				action: [ :aPresentation |
					aPresentation highlightEvaluateAndDo: [ :result | ] ]; 
				keymap: PharoShortcuts current doItShortcut; 
				icon: GLMUIThemeExtraIcons glamorousSpawn;
				yourself.
		GLMGenericAction new 
				title: 'Print it';
				action: [ :aPresentation |
					aPresentation printSelection ]; 
				keymap: PharoShortcuts current printItShortcut; 
				yourself.
		GLMGenericAction new 
				title: 'Inspect it'; 
				action: [ :aPresentation |
					aPresentation highlightEvaluateAndDo: [ :result | result inspect ] ]; 
				keymap: PharoShortcuts current inspectItShortcut; 
				icon: GLMUIThemeExtraIcons glamorousInspect;
				yourself.
		GLMGenericAction new 
				title: 'Basic Inspect it'; 
				action: [ :aPresentation |
					aPresentation highlightEvaluateAndDo: [ :result | result basicInspect ] ]; 
				shortcut: $I; 
				icon: GLMUIThemeExtraIcons glamorousInspect;
				yourself.
		GLMGenericAction new 
				title: 'Debug it'; 
				action: [ :aPresentation |
					aPresentation debugSelection ]; 
				shortcut: $D;
				yourself.
		GLMGenericAction new 
				title: 'Profile it'; 
				action: [ :aPresentation |
					aPresentation profileSelection ]; 
				withSeparatorAfter;
				yourself.
		 }
]

{ #category : #actions }
GLMRubricSmalltalkCodePresentation >> highlightEvaluateAndDo: aBlock [

	self announce: (GLMHighlightAndEvaluate new action: aBlock)
]

{ #category : #accessing }
GLMRubricSmalltalkCodePresentation >> highlightSmalltalkContext [

	^ highlightSmalltalkContext
		ifNil: [ nil ]
		ifNotNil: [ highlightSmalltalkContext glamourValue: self entity ]
]

{ #category : #'actions browsing' }
GLMRubricSmalltalkCodePresentation >> implementorsOfIt [

	self announce: (GLMTextEditorRequest for: #implementorsOfIt)
]

{ #category : #rendering }
GLMRubricSmalltalkCodePresentation >> installDefaultSelectionActions [

	self defaultSelectionActions do: [ :anAction |
		self addSelectionAction: anAction ]
]

{ #category : #actions }
GLMRubricSmalltalkCodePresentation >> printSelection [

	self announce: GLMPrintSelection
]

{ #category : #actions }
GLMRubricSmalltalkCodePresentation >> profileSelection [

	self announce: GLMProfileSelection
]

{ #category : #'actions browsing' }
GLMRubricSmalltalkCodePresentation >> referencesToIt [

	self announce: (GLMTextEditorRequest for: #referencesToIt)
]

{ #category : #'actions browsing' }
GLMRubricSmalltalkCodePresentation >> sendersOfIt [

	self announce: (GLMTextEditorRequest for: #sendersOfIt)
]

{ #category : #accessing }
GLMRubricSmalltalkCodePresentation >> smalltalkClass: aBlock [
	"aBlock takes as parameters the entity objects and 
	its evaluation is expected to produce a Smalltalk class"
	highlightSmalltalkContext := aBlock.
]

{ #category : #accessing }
GLMRubricSmalltalkCodePresentation >> variableBindings [
	^ (variableBindingsBlock glamourValue: self entity) ifNil: [OrderedCollection new]
]

{ #category : #accessing }
GLMRubricSmalltalkCodePresentation >> variableBindings: aBlock [
	^ variableBindingsBlock := aBlock
]
