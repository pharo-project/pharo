initialization
initialize

	| wrappingBlock |
	super initialize.	
	toSelectModel := self instantiate: ListModel.
	selectedModel := self instantiate: ListModel.
	selectedModel beMultipleSelection.
	toSelectModel beMultipleSelection.
	testPackage := OrderedCollection new.
	wrappingBlock := [ :package | package packageName ].
	toSelectModel displayBlock: wrappingBlock.
	selectedModel displayBlock: wrappingBlock.