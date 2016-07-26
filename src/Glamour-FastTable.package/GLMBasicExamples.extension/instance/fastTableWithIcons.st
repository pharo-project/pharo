*Glamour-FastTable
fastTableWithIcons
	<glmBrowser: 'Fast Table with icons' input: '($A to: $z)'>
	"self new tableWithIcons openOn: ($A to: $z)"
	|browser |
	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [ :a |
		a act: [:presentation | presentation inspect] entitled: 'Inspect'. 
		a fastTable
			title: 'Table with actions';
			act: [:presentation | presentation inspect] entitled: 'Inspect'; 
			column: 'Character' evaluated: [ :each | each asString ];
			column: 'ASCII' evaluated: [ :each | each asInteger printString ];
			selectionAct: [:tree | tree selection inspect ] on: $i entitled: 'Inspect';
			icon: [:each | 
				each asInteger odd 
					ifTrue: [ GLMUIThemeExtraIcons glamorousRedCircle ] 
					ifFalse: [ GLMUIThemeExtraIcons glamorousGreenCircle ] ]].
	
	browser transmit 
		to: #two; 
		from: #one; 
		andShow: [:a | a text ].
	
	browser transmit
		from: #one port: #strongSelection;
		to: #two;
		andShow: [ :a | a text format: [ :item | 'STRONG: ', item asString ] ].
	
	^ browser