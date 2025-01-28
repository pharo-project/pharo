"
I am a simple benchmark class to test the TFFISameThread scheme
"
Class {
	#name : 'BenchTFFISameThread',
	#superclass : 'BenchTFFI',
	#category : 'ThreadedFFI-UFFI-Tests-Benchs',
	#package : 'ThreadedFFI-UFFI-Tests',
	#tag : 'Benchs'
}

{ #category : 'private' }
BenchTFFISameThread >> doCallWith: aFloat another: aDouble [

	^ self ffiCall: #(float sumAFloatAndADouble(float aFloat, double aDouble))
]

{ #category : 'private' }
BenchTFFISameThread >> doCallWithByteArray: aByteArray [

	self ffiCall: #(uint64 id_int(void* aByteArray))
]

{ #category : 'private' }
BenchTFFISameThread >> doCallWithExternalAddress: anExternalAddress [

	self ffiCall: #(uint64 id_int(void* anExternalAddress))
]

{ #category : 'private' }
BenchTFFISameThread >> doPointerVoid: aPointer [

	self ffiCall: #(void id_int(void* aPointer))
]

{ #category : 'private' }
BenchTFFISameThread >> doSumWithConstants [

	^ self ffiCall: #(int64 sum_int64_t(int 1, int 2))

	
]

{ #category : 'private' }
BenchTFFISameThread >> doSumWithPointers [

	^ self ffiCall: #(int64 sum_int64_t(void* 1, void* 0))
]

{ #category : 'private' }
BenchTFFISameThread >> doSumWithPointers2 [

	^ self ffiCall: #(int64 sum_int64_t(void* 1, nil))

	
]

{ #category : 'accessing' }
BenchTFFISameThread >> ffiLibrary [

	^ TFTestLibraryUsingSameThreadRunner uniqueInstance
]

{ #category : 'running' }
BenchTFFISameThread >> runCall [
	<script: 'self new runCall traceCr'>

	^ super runCall
]

{ #category : 'running' }
BenchTFFISameThread >> runCallWithByteArray [
	<script: 'self new runCallWithByteArray traceCr'>

	^ super runCallWithByteArray
]

{ #category : 'running' }
BenchTFFISameThread >> runCallWithExternalAddress [
	<script: 'self new runCallWithExternalAddress traceCr'>

	^ super runCallWithExternalAddress
]

{ #category : 'running' }
BenchTFFISameThread >> runCallWithOptimizedIntegerConstants [
	<script: 'self new runCallWithOptimizedIntegerConstants traceCr'>

	| return |
	^ [ return := self doSumWithPointers2. 
		 self assert: return = 1 ] bench
]

{ #category : 'running' }
BenchTFFISameThread >> runCallWithOptimizedLiterals [
	<script: 'self new runCallWithOptimizedLiterals traceCr'>

	| return |
	^ [ return := self doSumWithPointers. 
		 self assert: return = 1 ] bench
]

{ #category : 'running' }
BenchTFFISameThread >> runCallWithOptimizedLiterals2 [
	<script: 'self new runCallWithOptimizedLiterals2 traceCr'>

	| return |
	^ [ return := self doSumWithPointers2. 
		 self assert: return = 1 ] bench
]

{ #category : 'running' }
BenchTFFISameThread >> runCallWithPointerVoid [
	<script: 'self new runCallWithPointerVoid traceCr'>

	^ super runCallWithPointerVoid
]

{ #category : 'running' }
BenchTFFISameThread >> runCallWithPointerVoidWithByteArray [
	<script: 'self new runCallWithPointerVoidWithByteArray traceCr'>

	^ super runCallWithPointerVoidWithByteArray

]

{ #category : 'running' }
BenchTFFISameThread >> runDoSumWithConstants [
	<script: 'self new runDoSumWithConstants traceCr'>

	| return |
	^ [ return := self doSumWithConstants. 
		 self assert: return = 3 ] bench
]
