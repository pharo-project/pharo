tests
testBasicCommandWriteToStdin
"testBasicCommandWriteToStdin  fails because of what the documentation says:
	> **Important** We have found some problems when using regular files for the `stdin`. While we do not strictly forbid that, we recommend you do so only if you know very well what you are doing. Otherwise, use blocking pipes for `stdin` (default behavior).

"	
	Smalltalk platform isUnix
		ifFalse: [ super testBasicCommandWriteToStdin ]