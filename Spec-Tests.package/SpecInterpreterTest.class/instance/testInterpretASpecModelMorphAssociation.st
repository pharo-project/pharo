tests
testInterpretASpecModelMorphAssociation

	| spec model morph |
	model := AbstractWidgetModel new.
	spec := {#PluggableListMorph. #model:. #model}.
	morph := specInterpreterClass interpretASpec: spec model: model.
	
	self assert: (model widget == morph).