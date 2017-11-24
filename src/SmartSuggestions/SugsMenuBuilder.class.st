"
I'm the one who finds the best suggestion for your selection or poition in text.
I also add the options to the displayed menus.
"
Class {
	#name : #SugsMenuBuilder,
	#superclass : #Object,
	#category : #'SmartSuggestions-Menu'
}

{ #category : #builder }
SugsMenuBuilder class >> buildContextMenuOn: aBuilder [
	| context suggs |

	context := SugsAbstractContext contextFor: aBuilder model.
	suggs := self findSuggestionsFor: context.
	suggs do: [ :aSugg |
					aBuilder item: aSugg label;
						target: aSugg;
						selector: #execute;
						icon: aSugg icon ]	
]

{ #category : #builder }
SugsMenuBuilder class >> buildSmartActionsShortcuts: aBuilder [
	<keymap>

	(aBuilder shortcut: #smartSuggestions)
		category: #SmalltalkEditor
		default: $t meta
		do: [ :morph | morph smartSuggestions ]
		description: 'Browse suggestions based on selections'.

	(aBuilder shortcut: #smartSuggestions)
		category:  RubSmalltalkEditor name
		default: $t meta
		do: [ :morph | morph editor smartSuggestions ]
		description: 'Browse suggestions based on selections'.

]

{ #category : #private }
SugsMenuBuilder class >> findBestNodeFor: context [
	| root node |
	"since the ast is used to put links for break points reparsing it will lose all the 
	meta information such as breakpoints, therefore when the code is unchanged do not reparse it."
	context hasUnacceptedEdits
		ifTrue: [ 
			root := Smalltalk compiler
				source: context code;
				options: #(+ optionParseErrors);
				parse.
			context selectedClass ifNotNil: [ :theClass | root methodNode methodClass: theClass ].
			root doSemanticAnalysis ]
		ifFalse: [ root := context selectedMethod ast ].
	node := root bestNodeFor: context selectedInterval.
	context selectedNode: node.
	^ node
]

{ #category : #suggestions }
SugsMenuBuilder class >> findSuggestionsFor: context [	
	"Finds the best suggestions for the context passed as a parameter"
	| node suggestions |

	context selectedMethod ifNil: [ ^#() ]. "no support for class pane yet"
	node := self findBestNodeFor: context .
	node ifNil: [ ^OrderedCollection new ].
	suggestions := node ifNotNil: [ node suggestions ].
	suggestions do:[:sugg| sugg context: context].
	^suggestions select: [:sugg | sugg isValidForContext ]
	
	
]

{ #category : #show }
SugsMenuBuilder class >> showMenuFor: model [
	| suggs menu context |

	context := SugsAbstractContext contextFor: model.
	suggs := SugsMenuBuilder findSuggestionsFor: context.
	menu := UIManager default newMenuIn: model model for: model.
	menu addTitle: 'Suggestions > ' , context selectedNodeLabel.
	suggs do: [ :aSugg | (menu add: aSugg label target: aSugg selector: #execute) icon: aSugg icon ].
	context selectTheTextForTheNode.
	menu setInvokingView: self.
	menu invokeModal
]
