compiling
compile2method: sourceStream 
			"Compile code without logging the source in the changes file"
			^OpalCompiler new
						source: sourceStream;
						class: self class;			
						compile.
		