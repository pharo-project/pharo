examples
createLabel: hoverAction
	| label |

	label := GLMLabelBrick new
			enable;
			color: Color white;
			text: 'Hover me';
			vShrinkWrap;
			hShrinkWrap;
			yourself.
	
	
	label	when: #onMouseHover do: [ hoverAction value: label ].
	
	^ label