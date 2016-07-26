accessing
buildMessageSearch
	self 
		createSearchWith: '``@receiver -> ``@arg'
		selectors: #(#superSends #superSends )
		inClass: RBTransformationRuleTest