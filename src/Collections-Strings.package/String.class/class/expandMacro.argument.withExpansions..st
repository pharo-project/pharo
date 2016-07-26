formatting
expandMacro: macroType argument: argument withExpansions: expansions 
	macroType = $s ifTrue: [^expansions at: argument].
	macroType = $p ifTrue: [^(expansions at: argument) printString].
	macroType = $n ifTrue: [^String cr].
	macroType = $t ifTrue: [^String tab].
	self error: 'unknown expansion type'