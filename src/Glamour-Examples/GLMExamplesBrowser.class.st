"
self new  openOn: GLMBasicExamples
"
Class {
	#name : #GLMExamplesBrowser,
	#superclass : #GLMCompositePresentation,
	#category : #'Glamour-Examples'
}

{ #category : #'user interface' }
GLMExamplesBrowser class >> menuExamplesOn: aBuilder [
	"Add a custom menu item to the world menu"

	<worldMenu>
	(aBuilder item: #'Glamour Example Browser')
		iconName: #glamorousHelp;
		parent: #Help;
		order: 4.0;
		help: 'Open Glamour Example Browser.';
		action: [ self open ]
]

{ #category : #'user interface' }
GLMExamplesBrowser class >> open [
	<script>
	
	self new openOn: GLMBasicExamples
]

{ #category : #building }
GLMExamplesBrowser >> compose [
	"self new openOn: GLMBasicExamples"

	self tabulator with: [:browser | 
		browser
			column: #titles;
			column: #example span: 4.
		browser transmit
			to: #titles;
			andShow: [ :a | self exampleListIn: a ].
		browser transmit
			to: #example;
			fromOutsidePort: #entity;
			from: #titles;
			andShow: [ :a | self exampleIn: a ] ]
]

{ #category : #private }
GLMExamplesBrowser >> exampleBrowserForPragma: each in: aClass [
	| exampleBrowser wrapperBrowser |
	exampleBrowser := aClass new perform: each methodSelector.
	wrapperBrowser := GLMTabulator new.
	wrapperBrowser column: #theOuterPane.
	wrapperBrowser transmit
		fromOutsideEntityPort;
		to: #theOuterPane;
		andShow: [ :a | a custom: exampleBrowser ].
	^ wrapperBrowser
		startOn: (Smalltalk compiler evaluate: (each argumentAt: 2) logged: false)
]

{ #category : #building }
GLMExamplesBrowser >> exampleIn: composer [
	| dynamicPresentation |
	dynamicPresentation := composer dynamic
		title: 'Browser';
		display:
				[ :exampleClass :examplePragma | self exampleBrowserForPragma: examplePragma in: exampleClass ].
	self
		flag:
			'openTree is a method that depends on Roassal. It is not loaded in the core version of Glamour. A better solution is needed here for loading this actions maybe based on pragmas.'.
	(GLMBrowser canUnderstand: #openTree)
		ifTrue: [ dynamicPresentation
				act: [ :dynamic | 
					dynamic cachedPresentation presentations first panes first
						presentations first openTree ]
				entitled: 'View browser tree' ].
	composer pharoMethod
		title: 'Source code';
		act: [ :text :exampleClass :examplePragma | 
			Smalltalk tools browser
				openOnClass: exampleClass
				selector: examplePragma methodSelector ]
			icon: GLMUIThemeExtraIcons glamorousBrowse
			entitled: 'Browse';
		smalltalkClass: [ :exampleClass | exampleClass ];
		doItReceiver: [ :exampleClass | exampleClass ];
		display:
				[ :exampleClass :pragma | exampleClass sourceCodeAt: pragma methodSelector ]
]

{ #category : #building }
GLMExamplesBrowser >> exampleListIn: a [
	^ a list
		title: 'Examples';
		display: [ :exampleClass | exampleClass allExamples ];
		sorted: [ :x :y | (x argumentAt: 1) < (y argumentAt: 1) ];
		format: [ :each | each argumentAt: 1 ]
]
