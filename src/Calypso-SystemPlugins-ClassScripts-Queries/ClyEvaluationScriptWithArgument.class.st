"
I represent any methods with parametric pragma <script:>.
Pragma parameter is an actual expression which should be evaluated.

So for execution I extract the argument from pragma and evaluate it using compiler. I do not use method itself.
"
Class {
	#name : #ClyEvaluationScriptWithArgument,
	#superclass : #ClyClassScript,
	#category : #'Calypso-SystemPlugins-ClassScripts-Queries'
}

{ #category : #'script detection' }
ClyEvaluationScriptWithArgument class >> canBeProvidedByMethod: aMethod [
	"Parametric script can be used in any method. 
	It is retrieved from pragma argument and method itself is not used for execution"
	^ true
]

{ #category : #'script detection' }
ClyEvaluationScriptWithArgument class >> isImplementedByMethod: aMethod [
	^ aMethod hasPragmaNamed: #script:
]

{ #category : #execution }
ClyEvaluationScriptWithArgument >> executeOn: aClass [

	| pragma |
	pragma := implementorMethod pragmaAt: #script:.
	^ aClass compiler
		receiver: aClass instanceSide;
		evaluate: pragma arguments first
]

{ #category : #testing }
ClyEvaluationScriptWithArgument >> isImplementedByClass: aClass [
	
	^ aClass instanceSide includesBehavior: self implementorClass instanceSide
]
