others
morphIcons
	<glmBrowser: 'Morph icons' input: 'GLMUIThemeExtraIcons class selectors select: [:each | (each beginsWith: ''glamorous'') and: [(GLMUIThemeExtraIcons perform: each) isForm ]]'>
	"self new morphIcons openOn: (GLMUIThemeExtraIcons class selectors select: [:each | (each beginsWith: 'glamorous') and: [(GLMUIThemeExtraIcons perform: each) isForm ]])"
	| browser |
	browser := GLMTabulator new.
	browser column: #list; column: #preview.
	browser title: 'Glamorous Icons'.
	browser transmit to: #list; andShow: [ :a | 
		a list title: 'GLMUIThemeExtraIcons' ].
	browser transmit to: #preview; from: #list; andShow: [ :a |
		a morph
			title: [:symbol | symbol];
			display: [:symbol | 
				(GLMUIThemeExtraIcons perform: symbol) asMorph ] ].
	^ browser