private - uffi
shellExecute: lpOperation file: lpFile parameters: lpParameters directory: lpDirectory show: nShowCmd	
	
	^self ffiCall: #( 
			FFIConstantHandle ShellExecuteA(
     				0,
     				String* lpOperation,
         			String* lpFile,
     				String* lpParameters,
     				String* lpDirectory,
        			int nShowCmd)) module: #shell32