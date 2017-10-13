initialization
initialize

	| wrappingBlock |
	super initialize.	
	toSelectModel := self instantiate: ListPresenter.
	selectedModel := self instantiate: ListPresenter.
	selectedModel beMultipleSelection.
	toSelectModel beMultipleSelection.
	testPackage := OrderedCollection new.
	wrappingBlock := [ :package | package packageName ].
	toSelectModel displayBlock: wrappingBlock.
	selectedModel displayBlock: wrappingBlock.