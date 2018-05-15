"
This browser is used to browse the class structure of every object in the inspector.

Example:
self openOn: World
"
Class {
	#name : #GTSimpleMethodsBrowser,
	#superclass : #GLMCompositePresentation,
	#category : #'GT-Inspector'
}

{ #category : #building }
GTSimpleMethodsBrowser >> accumulatedSourceOf: m for: object in: composite [
	composite accumulator with: [ :acc | 
		acc show: [ :a |
			a title: [ :method | method selector ].
			self sourceOf: m for: object in: a ] ]
]

{ #category : #building }
GTSimpleMethodsBrowser >> classesIn: a [
	^ a fastList
		title: 'Class Hierarchy';
		display: [ :object | 
			| all |
			all := OrderedCollection new.
			object class withAllSuperclassesDo: [:each | all add: each ].
			all ];
"		initialize: [ :list | list selection: list entity class ];"
		format: [ :class | class name ];
		enableFilter;
		morphicSelectionAct: [ :list | Smalltalk tools browser openOnClass: list selection ]
			icon: GLMUIThemeExtraIcons glamorousBrowse
			on: $b
			entitled: 'Browse'
]

{ #category : #building }
GTSimpleMethodsBrowser >> compose [
	self title: 'Meta'.
	self tabulator with: [ :browser |
		browser
			row: [ :r | r column: #classes; column: #methods span: 3 ];
			row: #source.
		browser transmit
			to: #classes;
			andShow: [ :a | self classesIn: a ].
		browser transmit
			to: #classes port: #selection;
			transformed: [:each | each class ].
		browser transmit
			fromOutsidePort: #entity;
			from: #classes;
			to: #methods;
			andShow: [ :a | self methodsIn: a ].
		browser transmit
			from: #methods;
			fromOutsidePort: #entity;
			to: #source;
			when: [ :method | method notNil ];
			andShow: [ :a :method :object | self sourceOf: method for: object in: a ].
		browser transmit
			toOutsidePort: #selection;
			from: #methods port: #evaluatedObject.
		browser transmit
			toOutsidePort: #selection;
			from: #source port: #evaluatedObject.
	]
]

{ #category : #building }
GTSimpleMethodsBrowser >> evaluationAction [
	^ GLMGenericAction new
		action: [ :listPresentation :obj | 
					| result |
					result := obj perform: listPresentation selection selector.
					(listPresentation pane port: #evaluatedObject) value: result ];
		shortcut: $g;
		title: 'Do it and go';
		yourself
]

{ #category : #building }
GTSimpleMethodsBrowser >> methodActionsFor: aMethod [
	^ aMethod selector isUnary
		ifTrue: [ Array with: self evaluationAction ]
		ifFalse: [ #() ]
]

{ #category : #building }
GTSimpleMethodsBrowser >> methodsIn: a [
	^ a list
		title: 'Methods';
		display: [ :object :class | class methods collect: #asRingDefinition];
		filterOn: [ :text :each | (text , '*') match: each selector ];
		helpMessage:  'Wildcard filtering (e.g., "*pattern*")';
		tags: [ :method | method category ];
		sorted: [ :method1 :method2 | method1 selector < method2 selector ];
		format: [ :method | method selector ];
		morphicSelectionAct: [ :list :object :class | 
			Smalltalk tools browser 
				openOnClass: class 
				selector: list selection selector ]
			icon: GLMUIThemeExtraIcons glamorousBrowse
			on: $b
			entitled: 'Browse';
		dynamicActionsOnSelection: [ :list | self methodActionsFor: list selection ]
]

{ #category : #building }
GTSimpleMethodsBrowser >> sourceOf: method for: object in: composite [
	^ composite pharoMethod 
		display: [ :m | m sourceCode ];
		smalltalkClass: [ :m | m methodClass ];
		doItReceiver: [ object ];
		selectionPopulate: #evaluatedObject
			on: $g
			entitled: 'Do it and go'
			with: [ :text | Smalltalk compiler receiver: object; evaluate: text selectedText ];
		installDefaultSelectionActions;
		selectionAct: [ :text | self updateOuterPaneFrom: text for: method ]
			icon: GLMUIThemeExtraIcons glamorousAccept
			on: $s
			entitled: 'Accept'
]

{ #category : #private }
GTSimpleMethodsBrowser >> updateOuterPaneFrom: text for: method [
	"this is a utility method to tell the pane surrounding the browser to update.
	The reason we do that is to support the use case of updating the GTInspector from
	the editor by modifying code directly in the Inspector."
	
	| outerPane newMethod |
	newMethod := method methodClass compile: text text notifying: nil.
	outerPane := text pane browser pane.
	outerPane browser panes do: #update.
	self
		flag:
			'This is a huge hack because of so many reasons:
				- the presentation accesses the state of the outer browser directly
				- the presentation depends on the implementation of the browser 
				It should be solved through transmissions.
				
				But, it works for now and the browser is reasonably updated'.
	^ outerPane
		withAllPanesAndPresentationsDo: [ :child | 
			(child isKindOf: self class)
				ifTrue: [ 
					((child first paneNamed: #classes) port: #selection) value: method methodClass.
					((child first paneNamed: #methods) port: #selection)
						value:
							((child first paneNamed: #methods) presentations first displayedValue ifNotNil: [ :v | v detect: [ :each | each selector = newMethod ] ]) ] ]
]
