utils
removeScriptMethods
	(ScriptLoader organization listAtCategoryNamed: 'pharo - scripts') do: [:each | ScriptLoader removeSelector: each].