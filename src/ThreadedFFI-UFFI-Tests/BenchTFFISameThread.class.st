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

{ #category : 'accessing' }
BenchTFFISameThread >> ffiLibrary [

	^ TFTestLibraryUsingSameThreadRunner uniqueInstance
]

{ #category : 'running' }
BenchTFFISameThread >> runCall [
	<script: 'self new runCall traceCr'>

	^ super runCall
]
