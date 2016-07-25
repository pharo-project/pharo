tests - to be refined
testPatternCascade

	rewriter replace: 'self `;messages; foo: 4; `;messages1'
		with: 'self `;messages1; bar: 4; `;messages'.
	self compare: (rewriter
				executeTree: (RBParser 
							parseExpression: 'self foo; printString; foo: 4; bar. self foo: 4');
				tree)
		to: (RBParser 
				parseExpression: 'self bar; bar: 4; foo; printString. self foo:4')