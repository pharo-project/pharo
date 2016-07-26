others
taggedTree
	"self new taggedTree openOn: #(1 2 3 4 5)"
	|browser |

	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [ :a |
		a tree
			tags: [:each | 
				{each even ifTrue: ['even'] ifFalse: ['odd'].
				each even ifTrue: ['x'] ifFalse: ['y']}
				]].
	browser transmit to: #two; from: #one; andShow: [ :a |
		a text
			display: [:x | 'You have selected ', x printString ]].
	^ browser