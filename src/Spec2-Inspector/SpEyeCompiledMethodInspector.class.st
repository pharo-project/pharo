"
Specialized version of inspector showing bytecodes symbolic representation, ast, ir, literals.
"
Class {
	#name : #SpEyeCompiledMethodInspector,
	#superclass : #SpEyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
SpEyeCompiledMethodInspector class >> label [
	^ 'Compiled Method Inspector'
]

{ #category : #list }
SpEyeCompiledMethodInspector >> addAllInstVars: elements [
	"Useless for compiledMethod"
]

{ #category : #list }
SpEyeCompiledMethodInspector >> addInstancesVariable: elements [
	"Do nothing"
]

{ #category : #list }
SpEyeCompiledMethodInspector >> addSpecialFields: elements [

	elements add: (SpDynamicEyeElement 
			host: self object 
			label: 'ast' 
			value: [ self object ast ]).
			
	elements add: (SpDynamicEyeElement 
			host: self object 
			label: 'ir' 
			description: [ :value | | string |
				string := value longPrintString.
				string copyFrom: 2 to: string size ]
			value: [ self object ir ]).

	elements add: (SpDynamicEyeElement 
			host: self object 
			label: 'all bytecodes' 
			value: [ self object symbolic ]).

	elements add: (SpDynamicEyeElement 
			host: self object 
			label: 'header' 
			value: [ self object headerDescription ]).
			
	self object literals withIndexDo: [ :literal :index|
		elements add: (SpDynamicEyeElement 
				host: self object 
				label: 'literal ', index asString 
				value: literal) ].
			
]

{ #category : #list }
SpEyeCompiledMethodInspector >> addVariableFields: elements [
	
	self object initialPC to: self object size do: [ :index |
		elements add: (SpDynamicEyeElement 
				host: self object 
				label: 'bc ', index asString 
				value: (self object at: index) asString)].
]

{ #category : #accessing }
SpEyeCompiledMethodInspector >> title [
	^ self object printString 
]
