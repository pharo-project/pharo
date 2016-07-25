testing
testAllCurrentVersionsContainsScriptLoader
	"self debug: #testAllCurrentVersionsContainsScriptLoader"
	
	| p |
	p := ScriptLoader new allCurrentVersions.
	self assert: (p anySatisfy: [ :pa | 'ScriptLoader*'  match: pa])