*Glamour-FastTable
fastTreeWithColumnsSimple
	"self new fastTreeWithColumnsSimple openOn: (1 to: 10)"

	<glmBrowser: 'Fast Tree With Columns' input: '(1 to: 10)'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastTreeTable 
			children: [ :x | 1 to: x-1 ];
			column: [:x | 'Numbers from 1 to ', x asString] evaluated: #asString;
			column: 'Even' evaluated: [ :each | each even asString ];
			column: 'Odd' evaluated: [ :each | each odd asString ];
			icon: [ :each | 
				each asInteger odd
					ifTrue: [ GLMUIThemeExtraIcons glamorousRedCircle ]
					ifFalse: [ GLMUIThemeExtraIcons glamorousGreenCircle ] ] ].
	^ browser