actions examples
treeWithMenu
	<glmBrowser: 'Tree with menu' input: '#(($a $b $c $d) ($e $f) ($g $h $i $j $l))'>
	"self new treeWithMenu openOn: #(($a $b $c $d) ($e $f) ($g $h $i $j $l))"
	|browser |
	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [ :a |
		a tree
			children: [:x :i | x asString size > 1 ifTrue: [x] ifFalse: [OrderedCollection new] ];
			selectionAct: [:tree | tree inspect ] on: $i entitled: 'Inspect';
			icon: [:x | GLMUIThemeExtraIcons glamorousGrayCircle ]].
	browser transmit to: #two; from: #one; andShow: [:a | a text ].
	^ browser