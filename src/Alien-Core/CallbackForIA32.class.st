"
A CallbackForIA32 is a callback for platforms with x86 processors using the System V ABI.
"
Class {
	#name : #CallbackForIA32,
	#superclass : #Callback,
	#category : #'Alien-Core'
}

{ #category : #accessing }
CallbackForIA32 class >> abi [
	^'IA32'
]

{ #category : #'class initialization' }
CallbackForIA32 class >> isForCurrentPlatform [
	| processor |
	processor := (SmalltalkImage current getSystemAttribute: 1003) asLowercase.
	^processor first = $i and: [processor = 'intel' or: ['i#86' match: processor]]
]

{ #category : #signatures }
CallbackForIA32 >> hwndlparamRetbool: callbackContext sp: spAlien [
	<signature: #(BOOL CALLBACK (*)(HWND, LPARAM))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (spAlien unsignedLongAt: 1))
			value: (spAlien unsignedLongAt: 5)) asAlien "i.e. coerce true to 1 and false to 0"
]

{ #category : #signatures }
CallbackForIA32 >> intcharstarRetint: callbackContext sp: spAlien [
	<signature: #(int (*)(int, char *))>
	^callbackContext wordResult:
		(block
			value: (spAlien signedLongAt: 1)
			value: (Alien forPointer: (spAlien unsignedLongAt: 5)))
]

{ #category : #signatures }
CallbackForIA32 >> logfontstartextmetricstardwordlparamRetint: callbackContext sp: spAlien [
	<signature: #(int CALLBACK (*)(const LOGFONT *, const TEXTMETRIC *, DWORD, LPARAM))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (spAlien unsignedLongAt: 1))
			value: (Alien forPointer: (spAlien unsignedLongAt: 5))
			value: (spAlien unsignedLongAt: 9)
			value: (spAlien signedLongAt: 13))
]

{ #category : #evaluation }
CallbackForIA32 >> valueInContext: callbackContext [ "<VMCallbackContext32|VMCallbackContext64> ^<Integer>"
	numEvaluatorArgs = 2 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext stackp].
	numEvaluatorArgs = 3 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext stackp
			with: callbackContext intregargsp].
	numEvaluatorArgs = 4 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext stackp
			with: callbackContext intregargsp
			with: callbackContext floatregargsp].
	numEvaluatorArgs = 1 ifTrue: "legacy support"
		[^self perform: evaluator
			with: callbackContext].
	self error: 'invalid evaluator argument count'
]

{ #category : #signatures }
CallbackForIA32 >> voidstarvoidstarRetint: callbackContext sp: spAlien [
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (spAlien unsignedLongAt: 1))
			value: (Alien forPointer: (spAlien unsignedLongAt: 5)))
]
