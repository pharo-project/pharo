tests
testBuildWidgetForWithSpec
	| model morph |
	model := TestingComposablePresenter new.
	morph := model buildWithSpec: #testingSpec.
	self assert: morph model == model.
	self assert: model widget class = RubPluggableTextMorph