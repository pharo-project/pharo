examples
exampleWithCustomClickAction
	| text action selection |
	text := 'abciuwhfi whifu wehifuhweifu wi ' asTxModel.
	action := TxActionAttribute new 
			filter: [ :evt | evt commandKeyPressed and: [ evt isMouseDown ] ];
			value: [ self inform: 'action' ].
	selection := text startPosition selectTo: (text startPosition + 10).
	selection
		applyAttribute: action;
		applyAttribute: (TxForeColorAttribute with: Color red).
	^ self openInWindowWithText: text