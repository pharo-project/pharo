ui
gtInspectorActionEvaluateResultAndGo
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ :p | p selection: self result returnValue ];
		icon: (Smalltalk ui icons iconNamed: #glamorousRight);
		title: 'Evaluate result and go'