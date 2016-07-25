builder
buildContextMenuOn: aBuilder
	| context suggs |

	context := SugsAbstractContext contextFor: aBuilder model.
	suggs := self findSuggestionsFor: context.
	suggs do: [ :aSugg |
					aBuilder item: aSugg label;
						target: aSugg;
						selector: #execute;
						icon: aSugg icon ]	