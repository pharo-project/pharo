Extension { #name : #WinPlatform }

{ #category : #'*Native-Browser' }
WinPlatform >> privShellExecute: lpOperation file: lpFile parameters: lpParameters directory: lpDirectory show: nShowCmd [
	^ self ffiCall: #( 
			FFIConstantHandle ShellExecuteA(
     				0,
     				char* lpOperation,
         			char* lpFile,
     				char* lpParameters,
     				char* lpDirectory,
        			int nShowCmd)) module: #shell32
]

{ #category : #'*Native-Browser' }
WinPlatform >> privateOpenNativeBrowserOn: pathString [
	^ self privShellExecute: 'explore' file: pathString parameters: '' directory: '' show: 5 "SW_SHOW"
]
