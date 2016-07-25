tests
testExpand
	| model selectors |
	model := NECTypedModel class: NECTestClass.
	self assert: (model entriesOfType: #locals) isEmpty.
	self assert: (model entriesOfType: #instance) isEmpty.
	model toggleExpand.
	selectors := model entriesOfType: #selector.
	self assert: selectors size == 6.
	self assert: (selectors includes: #initialize).
	self assert: (selectors includes: #lowPriorityOverrides:).
	self assert: (selectors includes: #testIt:).
	model toggleExpand.
	selectors := model entriesOfType: #selector.
	self assert: selectors size > 100.
	self assert: (selectors includes: #instVarAt:).
	self assert: (selectors includes: #initialize).
	self assert: (selectors includes: #lowPriorityOverrides:).
	self assert: (selectors includes: #testIt:).
	self assert: (selectors includes: #instVarAt:).
	model toggleExpand.
	selectors := model entriesOfType: #selector.
	self assert: selectors size == 6