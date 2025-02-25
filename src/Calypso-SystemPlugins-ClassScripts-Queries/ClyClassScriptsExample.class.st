"
I am example class which includes all kind of scripts. such as class initalization, <example>, <sampleInstance>, <script>, <script:>...
Look at the class side
"
Class {
	#name : 'ClyClassScriptsExample',
	#superclass : 'Object',
	#category : 'Calypso-SystemPlugins-ClassScripts-Queries',
	#package : 'Calypso-SystemPlugins-ClassScripts-Queries'
}

{ #category : 'class initialization' }
ClyClassScriptsExample class >> initialize [
	self inform: 'it is class initialization example from ', self name
]

{ #category : 'methods with examples' }
ClyClassScriptsExample class >> methodWithExample [
	<example>
	self inform: 'it is example string from ', self name.

	^'it is example string'
]

{ #category : 'methods with samples' }
ClyClassScriptsExample class >> methodWithSample [
	<sampleInstance>

	^'it is string sample instance from ', self name
]

{ #category : 'methods with scripts' }
ClyClassScriptsExample class >> methodWithScript [
	<script>
	self inform: 'It is example method with script from ', self name
]

{ #category : 'methods with scripts' }
ClyClassScriptsExample class >> methodWithScriptWithArgument [
	<script: 'self inform: ''It is example method with script with argument from '', self name'>
]

{ #category : 'methods with script' }
ClyClassScriptsExample >> instSideMethodWithScriptWithArgument [
	<script: 'self inform: ''It is example inst side method with script with argument from '', self name'>
]

{ #category : 'see class side' }
ClyClassScriptsExample >> seeClassSide [

	
]
