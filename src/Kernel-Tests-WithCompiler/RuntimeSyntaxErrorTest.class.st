Class {
	#name : 'RuntimeSyntaxErrorTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Tests-WithCompiler',
	#package : 'Kernel-Tests-WithCompiler'
}

{ #category : 'tests' }
RuntimeSyntaxErrorTest >> testFaultyInstalledMethod [
	<ignoreNotImplementedSelectors: #(faultyMethodExample)>
	| method |
	method := self class compiler
		          permitFaulty: true;
		          install: 'faultyMethodExample 1+¿'.

	[ self faultyMethodExample ]
		on: OCRuntimeSyntaxError
		do: [ :error |
			| ctx |
			self assert: error class equals: OCRuntimeSyntaxError.
			self assert: error messageText equals: 'Unknown character'.
			self
				assert: error description
				equals: 'OCRuntimeSyntaxError: Unknown character'.
			ctx := error signalContext filterDebuggerStack.
			self assert: ctx sourceNodeExecuted sourceCode equals: '¿' ].

	method removeFromSystem
]

{ #category : 'tests' }
RuntimeSyntaxErrorTest >> testFaultyScript [

	[
	OpalCompiler new
		permitFaulty: true;
		evaluate: '1+¿' ]
		on: OCRuntimeSyntaxError
		do: [ :error |
			| ctx |
			self assert: error class equals: OCRuntimeSyntaxError.
			self assert: error messageText equals: 'Unknown character'.
			self
				assert: error description
				equals: 'OCRuntimeSyntaxError: Unknown character'.
			ctx := error signalContext filterDebuggerStack.
			self assert: ctx sourceNodeExecuted sourceCode equals: '¿' ]
]

{ #category : 'tests' }
RuntimeSyntaxErrorTest >> testFaultyUninstalledMethod [

	| method |
	method := OpalCompiler new
		          permitFaulty: true;
		          compile: 'foo 1+¿'.

	[ nil executeMethod: method ]
		on: OCRuntimeSyntaxError
		do: [ :error |
			| ctx |
			self assert: error class equals: OCRuntimeSyntaxError.
			self assert: error messageText equals: 'Unknown character'.
			self
				assert: error description
				equals: 'OCRuntimeSyntaxError: Unknown character'.
			ctx := error signalContext filterDebuggerStack.
			self assert: ctx sourceNodeExecuted sourceCode equals: '¿' ]
]
