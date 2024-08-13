Class {
	#name : 'LibTTYTest',
	#superclass : 'TestCase',
	#category : 'UnifiedFFI-Tests-Libraries',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Libraries'
}

{ #category : 'asserting' }
LibTTYTest >> assertProcessSpawnedWithFileDescriptor: fileDescriptor path: path arguments: arguments environment: environment input: inputString writes: expectedOutputString hasStatus: expectedStatus [

	| pid status writeValue inputByteArray outputByteArray |

	pid := -1.
	status := nil.
	outputByteArray :=
		[
			pid := LibTTY uniqueInstance ttySpawn: fileDescriptor path: path
				arguments: arguments
				environment: environment.
			self deny: pid equals: -1.
			inputString ifNotEmpty: [
				inputByteArray := inputString utf8Encoded.
				writeValue := LibC uniqueInstance write: fileDescriptor buffer: inputByteArray size: inputByteArray size.
				self assert: writeValue equals: inputByteArray size ].
			self readOutput: fileDescriptor
		] ensure: [
			LibC uniqueInstance close: fileDescriptor.
			status := pid ~= -1 ifTrue: [ self getProcessStatus: pid ] ].
	self assert: outputByteArray utf8Decoded equals: expectedOutputString.
	self assert: status equals: expectedStatus.
]

{ #category : 'asserting' }
LibTTYTest >> assertProcessSpawnedWithPseudoTerminalPath: path arguments: arguments environment: environment input: inputString writes: expectedOutputString hasStatus: expectedStatus [

	self assertProcessSpawnedWithFileDescriptor: self openPseudoTerminal
		path: path
		arguments: arguments
		environment: environment
		input: inputString
		writes: expectedOutputString
		hasStatus: expectedStatus
]

{ #category : 'asserting' }
LibTTYTest >> assertShellProcessWithCommand: shellCommand hasStatus: expectedStatus [

	self assertShellProcessWithCommand: shellCommand writes: '' hasStatus: expectedStatus
]

{ #category : 'asserting' }
LibTTYTest >> assertShellProcessWithCommand: shellCommand input: inputString writes: expectedOutputString hasStatus: expectedStatus [

	self assertProcessSpawnedWithPseudoTerminalPath: '/bin/sh'
		arguments: { '/bin/sh'. '-c'. shellCommand }
		environment: Smalltalk os environment
		input: inputString
		writes: expectedOutputString
		hasStatus: expectedStatus
]

{ #category : 'asserting' }
LibTTYTest >> assertShellProcessWithCommand: shellCommand writes: expectedOutputString hasStatus: expectedStatus [

	self assertShellProcessWithCommand: shellCommand input: '' writes: expectedOutputString hasStatus: expectedStatus
]

{ #category : 'private' }
LibTTYTest >> flagOpenRDWR [

	^ 2
]

{ #category : 'private' }
LibTTYTest >> getProcessStatus: pid [

	| statusArray |

	statusArray := FFIExternalArray externalNewType: 'int' size: 1.
	^ [
		| pid2 statusInteger upper lower |
		pid2 := LibC uniqueInstance waitpid: pid status: statusArray getHandle options: 0.
		pid2 = pid ifFalse: [
			Error signal: 'Could not get process status' ].
		statusInteger := statusArray first.
		upper := (statusInteger bitShift: -8) bitAnd: 16rFF.
		lower := statusInteger bitAnd: 16r7F.
		(lower > 0) ifTrue: [ 128 + lower ] ifFalse: [ upper ]
	] ensure: [
		statusArray free ]
]

{ #category : 'private' }
LibTTYTest >> openPseudoTerminal [

	| fdm |

	(fdm := LibC uniqueInstance posix_openpt: self flagOpenRDWR) ~= -1 ifFalse: [
		Error signal: 'Could not open pseudo-terminal device' ].
	(LibC uniqueInstance grantpt: fdm) ~= -1 ifFalse: [
		Error signal: 'Could not grant access to the slave pseudo-terminal device' ].
	(LibC uniqueInstance unlockpt: fdm) ~= -1 ifFalse: [
		Error signal: 'Could not unlock the slave pseudo-terminal device' ].
	^ fdm
]

{ #category : 'private' }
LibTTYTest >> performTest [

	Smalltalk os isWindows ifTrue: [
		self skip ].
	super performTest
]

{ #category : 'private' }
LibTTYTest >> readOutput: fdm [

	^ ExternalAddress allocate: 1024 bytesDuring: [ :buffer |
		ByteArray streamContents: [ :stream |
			| count |
			[ (count := LibC uniqueInstance read: fdm buffer: buffer size: buffer size) > 0 ] whileTrue: [
				1 to: count do: [ :index |
					stream nextPut: (buffer byteAt: index) ] ] ] ]
]

{ #category : 'tests' }
LibTTYTest >> test1 [

	| path fileDescriptorPseudoTerminal template fileDescriptorFile |
		
	path := (#('/bin' '/usr/bin') collect: [ :element | element , '/true' ])
		detect: [ :element | element asFileReference exists ].

	fileDescriptorPseudoTerminal := self openPseudoTerminal.
	self assertProcessSpawnedWithFileDescriptor: fileDescriptorPseudoTerminal
		path: path
		arguments: { path }
		environment: Smalltalk os environment
		input: ''
		writes: ''
		hasStatus: 0.

	(template := ExternalAddress fromString: '/tmp/file.XXXXXX')
		autoRelease.
	fileDescriptorFile := LibC uniqueInstance mkstemp: template.
	self deny: fileDescriptorFile equals: -1.
	[
		self assertProcessSpawnedWithFileDescriptor: fileDescriptorFile
			path: path
			arguments: { path }
			environment: Smalltalk os environment
			input: ''
			writes: ''
			hasStatus: 127
	] ensure: [
		template utf8StringFromCString asFileReference delete ].
]

{ #category : 'tests' }
LibTTYTest >> test2 [

	self assertProcessSpawnedWithPseudoTerminalPath: '/bin/echo'
		arguments: #('/bin/echo' 'Argument1' 'Argument2' 'Argument3')
		environment: Smalltalk os environment
		input: ''
		writes: 'Argument1 Argument2 Argument3' , String crlf
		hasStatus: 0.

	self assertProcessSpawnedWithPseudoTerminalPath: '/bin/echo'
		arguments: #('/bin/echo' 'ĀĂĄ')
		environment: Smalltalk os environment
		input: ''
		writes: 'ĀĂĄ' , String crlf
		hasStatus: 0.

	self assertProcessSpawnedWithPseudoTerminalPath: '/usr/bin/env'
		arguments: #('/usr/bin/env')
		environment: (OrderedDictionary with: 'VAR1' -> 'value1' with: 'VAR2' -> 'value2' with: 'VAR3' -> 'value3')
		input: ''
		writes: (String crlf join: #('VAR1=value1' 'VAR2=value2' 'VAR3=value3' ''))
		hasStatus: 0.

	self assertProcessSpawnedWithPseudoTerminalPath: '/usr/bin/env'
		arguments: #('/usr/bin/env')
		environment: (OrderedDictionary with: 'VAR' -> 'ĀĂĄ')
		input: ''
		writes: (String crlf join: #('VAR=ĀĂĄ' ''))
		hasStatus: 0.

	self assertProcessSpawnedWithPseudoTerminalPath: '/usr/bin/head'
		arguments: #('/usr/bin/head' '-n' '2')
		environment: Smalltalk os environment
		input: (String lf join: #('Line1' 'Line2' 'Line3' ''))
		writes: (String crlf join: #('Line1' 'Line2' 'Line3' 'Line1' 'Line2' ''))
		hasStatus: 0.

	self assertProcessSpawnedWithPseudoTerminalPath: '/bin/doesnotexist'
		arguments: #('/bin/doesnotexist')
		environment: Smalltalk os environment
		input: ''
		writes: 'Error in tty_spawn at execve(path, argv, envp): No such file or directory' , String crlf
		hasStatus: 127.
]

{ #category : 'tests' }
LibTTYTest >> test3 [

	| formatDictionary echoesControlCharacters |

	self assertShellProcessWithCommand: 'exit 42'
		input: ''
		writes: ''
		hasStatus: 42.

	formatDictionary := Dictionary <- {
		'lf' -> Character lf.
		'crlf' -> String crlf.
		'end' -> Character end.
		'bs' -> Character backspace }.
	echoesControlCharacters := OSPlatform current isMacOSX.
	self assertShellProcessWithCommand: 'tail -n 1'
		input: ('Line 1{lf}Line 2{lf}{end}' format: formatDictionary)
		writes: ('Line 1{crlf}Line 2{crlf}{echoedend}Line 2{crlf}' format: formatDictionary ,
			(Dictionary with: 'echoedend' ->
				(echoesControlCharacters ifTrue: [ '^D{bs}{bs}' format: formatDictionary ] ifFalse: [ '' ])))
		hasStatus: 0.
]

{ #category : 'tests' }
LibTTYTest >> test4 [

	self assertShellProcessWithCommand: 'printf FOO'
		writes: 'FOO'
		hasStatus: 0.
	self assertShellProcessWithCommand: 'printf BAR 1>&2'
		writes: 'BAR'
		hasStatus: 0.
	self assertShellProcessWithCommand: 'exit 123'
		writes: ''
		hasStatus: 123.
]

{ #category : 'tests' }
LibTTYTest >> test5 [

	self assertShellProcessWithCommand: 'true'
		hasStatus: 0.
	self assertShellProcessWithCommand: '! true'
		hasStatus: 1.
	self assertShellProcessWithCommand: 'test -t 0'
		hasStatus: 0.
	self assertShellProcessWithCommand: 'test -t 0 </dev/zero'
		hasStatus: 1.
	self assertShellProcessWithCommand: 'test -t 1'
		hasStatus: 0.
	self assertShellProcessWithCommand: 'test -t 1 >/dev/null'
		hasStatus: 1.
	self assertShellProcessWithCommand: 'test -t 2'
		hasStatus: 0.
	self assertShellProcessWithCommand: 'test -t 2 2>/dev/null'
		hasStatus: 1.
	self assertShellProcessWithCommand: 'kill -s KILL $$'
		hasStatus: 128 + 9.
]
