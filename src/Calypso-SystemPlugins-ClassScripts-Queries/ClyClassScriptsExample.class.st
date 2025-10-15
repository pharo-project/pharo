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

{ #category : 'methods with examples' }
ClyClassScriptsExample class >> methodWithExample [
	"
	Don't use self inform: it does not exist anymore
	But you can get the logic of the pragma.
	
	<example>
	^ 'it is example string from ', self name.
"
]

{ #category : 'methods with samples' }
ClyClassScriptsExample class >> methodWithSample [
	"
Don't use self inform: it does not exist anymore
	But you can get the logic of the pragma.
	
	<sampleInstance>

	^'it is string sample instance from ', self name"
]

{ #category : 'methods with scripts' }
ClyClassScriptsExample class >> methodWithScript [
	"
	Don't use self inform: it does not exist anymore
	But you can get the logic of the pragma.
	
	<script>
	self inform: 'It is example method with script from ', self name"
]

{ #category : 'methods with scripts' }
ClyClassScriptsExample class >> methodWithScriptWithArgument [
	"
	Don't use self inform: it does not exist anymore
	But you can get the logic of the pragma.
	
	<script: 'self inform: ''It is example method with script with argument from '', self name'>"
]

{ #category : 'methods with script' }
ClyClassScriptsExample >> instSideMethodWithScriptWithArgument [
	"
	Don't use self inform: it does not exist anymore
	But you can get the logic of the pragma.
	
	
	<script: 'self inform: ''It is example inst side method with script with argument from '', self name'>
	"
]

{ #category : 'see class side' }
ClyClassScriptsExample >> seeClassSide [

	
]
