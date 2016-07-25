accessing
allRules

	^ rules ifNil: [
		rules := RBCompositeLintRule allGoodRules leaves
			reject: [ :rule |
				self excludedRuleNames includes: rule class name ] ]