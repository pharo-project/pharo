text
populatePortAction
	<glmBrowser: 'Populate port action' input: 'String withAll: ''Select a piece of text and open on the next pane via the contextual menu or by pressing Modifier+o. Otherwise just open the complete copy of the current text on the next pane via the menu or by pressing Modifier+l.'''>
	"self new populatePortAction openOn: 'Select a piece of text and open on the next pane either with the overall button, or via the context menu'"
	| browser |
	browser := GLMFinder new.
	browser show: [:a | a text
		selectionPopulate: #selection on: $o entitled: 'Open selection on next pane' with: [:text | text selectedText ];
		populate: #selection icon: GLMUIThemeExtraIcons glamorousRight on: $l entitled: 'Open complete text on next pane' with: [:text | text text ]].
	^ browser 