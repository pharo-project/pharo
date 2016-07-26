actions examples
multipleActions
	<glmBrowser: 'Multiple actions' input: '''This example shows how multiple actions are rendered.'''>
	"self new multipleActions openOn: 'This example shows how multiple actions are rendered.'"
	|browser |
	browser := GLMTabulator new.
	browser act: [:b | b inspect ] on: $/ entitled: 'Global action with shortcut'.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a |
		a text 
			title: 'Example';
			selectionAct: [:text | text inspect ] on: $i entitled: 'Selection action';
			act: [:text | text inspect ] entitled: 'Action without icon';
			act: [:text | text inspect ] icon: GLMUIThemeExtraIcons glamorousInspect entitled: 'Action with icon' ].
	^ browser