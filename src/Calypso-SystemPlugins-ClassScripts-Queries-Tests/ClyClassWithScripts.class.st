"
I am example class which includes all kind of scripts.
Look at the class side
"
Class {
	#name : #ClyClassWithScripts,
	#superclass : #Object,
	#category : #'Calypso-SystemPlugins-ClassScripts-Queries-Tests'
}

{ #category : #'class initialization' }
ClyClassWithScripts class >> initialize [
	self inform: 'it is class initialization example from ', self name
]

{ #category : #'methods with examples' }
ClyClassWithScripts class >> methodWithExample [
	<example>
	self inform: 'it is example string from ', self name.
	
	^'it is example string'
]

{ #category : #'methods with samples' }
ClyClassWithScripts class >> methodWithSample [
	<sampleInstance>
	
	^'it is string sample instance from ', self name
]

{ #category : #'methods with scripts' }
ClyClassWithScripts class >> methodWithScript [
	<script>
	self inform: 'It is example method with script from ', self name
]

{ #category : #'methods with scripts' }
ClyClassWithScripts class >> methodWithScriptWithArgument [
	<script: 'self inform: ''It is example method with script with argument from '', self name'>
	
]

{ #category : #'methods with script' }
ClyClassWithScripts >> instSideMethodWithScriptWithArgument [
	<script: 'self inform: ''It is example inst side method with script with argument from '', self name'>
]
