accessing-compiler
renamedMethodCopyOf: aMethod selector: aSymbol
	| newMethod |
	newMethod := aMethod ast
		selector: aSymbol
		yourself.
	"#compiledMethod answers a new instance"
	^ newMethod compiledMethod