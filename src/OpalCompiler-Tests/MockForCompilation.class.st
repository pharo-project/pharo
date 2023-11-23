"
A simple mock with an ivar used by tests to check compilation, instance variable shadowing and other
"
Class {
	#name : 'MockForCompilation',
	#superclass : 'Object',
	#instVars : [
		'var1'
	],
	#category : 'OpalCompiler-Tests-FromOld',
	#package : 'OpalCompiler-Tests',
	#tag : 'FromOld'
}

{ #category : 'accessing' }
MockForCompilation >> var1 [

	^ var1
]
