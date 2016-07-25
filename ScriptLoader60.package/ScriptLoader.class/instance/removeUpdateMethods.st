utils
removeUpdateMethods
	(ScriptLoader organization listAtCategoryNamed: 'pharo - updates') do: [:each | ScriptLoader removeSelector: each].