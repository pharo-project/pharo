building
logChildren: anInteger triggerReference: triggerReference

	1 to: anInteger do: [ :index |
		self 
			logEvent: (EpExpressionEvaluation expression: index asString)
			triggerReference: triggerReference ].