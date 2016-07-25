tabs examples
tabsWithDifferentActions
	<glmBrowser: 'Tabs with different actions' input: '42'>
	"self new tabsWithDifferentActions openOn: 42"
	
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [:a | 
		a text
			display: 'Tab with multiple actions. Switch between tabs to check the toolbars on the right';
			title: 'Tab with multiple actions';
			act: [:x | x inspect] icon: GLMUIThemeExtraIcons glamorousInspect entitled: 'Inspect with icon';
			act: [:x | x printString inspect] icon: GLMUIThemeExtraIcons glamorousBrowse entitled: 'Print it and inspect with icon';
			act: [:x | x inspect] entitled: 'Inspect without icon';
			act: [:x | x printString inspect] entitled: 'Print it and inspect without icon'.
		a text 
			display: 'Tab without actions. Switch between tabs to check the toolbars on the right';
			title: 'Tab without actions' ].
	^ browser