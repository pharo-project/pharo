public access
decompileMethod: aCompiledMethod
	^ Smalltalk globals 
		at: #FBDDecompiler  
		ifPresent: [ :decompilerClass | [ decompilerClass new decompile: aCompiledMethod ]
			on: Error 
			do: [ RBMethodNode errorMethodNode: aCompiledMethod selector errorMessage: 'Decompilation failed'.  ] ]
		ifAbsent: [ RBMethodNode errorMethodNode: aCompiledMethod selector errorMessage: 'No decompiler available'. ]

