show
showMenuFor: model
	| suggs menu context |

	context := SugsAbstractContext contextFor: model.
	suggs := SugsMenuBuilder findSuggestionsFor: context.
	menu := UIManager default newMenuIn: model model for: model.
	menu addTitle: 'Suggestions > ' , context selectedNodeLabel.
	suggs do: [ :aSugg | (menu add: aSugg label target: aSugg selector: #execute) icon: aSugg icon ].
	context selectTheTextForTheNode.
	menu setInvokingView: self.
	menu invokeModal