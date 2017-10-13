tests
testInterpretASpecModelMorphAssociation

	| spec model morph |
	model := AbstractWidgetPresenter new.
	spec := {#PluggableListMorph. #model:. #model}.
	morph := specInterpreterClass interpretASpec: spec model: model.
	
	self assert: (model widget == morph).