accessing-compiler
renamedMethodCopyOf: aMethod selector: aSymbol
	| source |
	source := (aMethod methodClass decompilerClass new
		decompile: aSymbol
		in: aMethod methodClass
		method: aMethod) decompileString.
	"Fix compiler glitch"
	source := source
		copyReplaceAll: 'selector: #=>', String cr
		with: 'selector: #= >', String cr.
	^ (aMethod methodClass compilerClass new
		compile: source
		in: aMethod methodClass
		notifying: nil
		ifFail: []) generate