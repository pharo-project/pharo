*Glamour-FastTable
fastTreeWithIcon
	"self new fastTreeWithIcon openOn: (1 to: 10)"

	<glmBrowser: 'Fast Tree With Icon' input: '(1 to: 10)'>
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow:
				[ :a | 
			a fastTree
				title: 'Fast Tree';
				children: [ :x | 1 to: x - 1 ];
				icon: [ :each | 
					each asInteger odd
						ifTrue: [ GLMUIThemeExtraIcons glamorousRedCircle ]
						ifFalse: [ GLMUIThemeExtraIcons glamorousGreenCircle ] ] ].
	^ browser