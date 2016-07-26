actions examples
tableWithIcons
	<glmBrowser: 'Icons' input: '($A to: $z)'>
	"self new tableWithIcons openOn: ($A to: $z)"
	|browser |
	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [ :a |
		a act: [:presentation | presentation inspect] entitled: 'Inspect'. 
		a table
			title: 'Table with actions';
			act: [:presentation | presentation inspect] entitled: 'Inspect'; 
			column: 'Character' evaluated: [ :each | each asString ];
			column: 'ASCII' evaluated: [ :each | each asInteger printString ];
			selectionAct: [:tree | tree selection inspect ] on: $i entitled: 'Inspect';
			icon: [:each | each asInteger odd 
						ifTrue: [ GLMUIThemeExtraIcons glamorousRedCircle ] 
						ifFalse: [ GLMUIThemeExtraIcons glamorousGreenCircle ] ]].
	browser transmit to: #two; from: #one; andShow: [:a | a text ].
	^ browser