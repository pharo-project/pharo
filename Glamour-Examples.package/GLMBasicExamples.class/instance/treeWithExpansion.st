others
treeWithExpansion
	<glmBrowser: 'Tree with expansion' input: '#(($a $b $c $d) ($e $f) ($g $h $i $j ($l $m $n)))'>
	"self new treeWithExpansion openOn: #(($a $b $c $d) ($e $f) ($g $h $i $j ($l $m $n)))"
	|browser |
	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [:a |
		a tree
			title: 'No expansion';
			children: [:x :i | x asString size > 1 ifTrue: [x] ifFalse: [OrderedCollection new] ].
		a tree
			title: 'Roots expanded';
			rootsExpanded;
			children: [:x :i | x asString size > 1 ifTrue: [x] ifFalse: [OrderedCollection new] ].
		a tree
			title: 'All expanded';
			allExpanded;
			children: [:x :i | x asString size > 1 ifTrue: [x] ifFalse: [OrderedCollection new] ] ].
	browser transmit to: #two; from: #one; andShow: [ :a | a text ].
	^ browser