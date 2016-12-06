ast manipulation
wrapCondition: hook link: aLink
	| linkBlock conditionExpression condSelector condArguments linkConditionArguments|
	
	condSelector := aLink condition valueSelector.
	linkConditionArguments  := aLink conditionArguments.
	condArguments := #().
	
	(condSelector numArgs > 0) ifTrue: [
			condArguments := OrderedCollection new.
			1 to: condSelector numArgs do: [:i |
				condArguments add: (self genReification: (linkConditionArguments at: i) for: aLink)]
	].
	
	linkBlock := self encloseInBlock: hook asSequenceNode.
	conditionExpression := RBMessageNode 
			receiver: ((self hasOption: #optionInlineCondition for: aLink)  
				ifTrue: [aLink condition asLiteralNode]
				ifFalse:[RBMessageNode receiver: (RBLiteralNode value: aLink)
											 selector: #condition ])
			selector: condSelector
			arguments: condArguments.
	^RFMessageNode
				receiver: conditionExpression
				selector: #ifTrue:
				arguments: {linkBlock}