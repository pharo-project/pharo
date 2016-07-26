testing
testAllCurrentPackagesContainScriptLoader
	"self debug: #testAllCurrentPackagesContainScriptLoader"

	| p |
	p := ScriptLoader new allCurrentPackages collect: [:each | each package name].
	self assert: (p anySatisfy: [ :pa | 'ScriptLoader*' match: pa])