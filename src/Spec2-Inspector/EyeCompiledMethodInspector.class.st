"
Specialized version of inspector showing bytecodes symbolic representation, ast, ir, literals.
"
Class {
	#name : #EyeCompiledMethodInspector,
	#superclass : #EyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
EyeCompiledMethodInspector class >> label [
	^ 'Compiled Method Inspector'
]

{ #category : #list }
EyeCompiledMethodInspector >> addAllInstVars: elements [
	"Useless for compiledMethod"
]

{ #category : #list }
EyeCompiledMethodInspector >> addInstancesVariable: elements [
	"Do nothing"
]

{ #category : #list }
EyeCompiledMethodInspector >> addSpecialFields: elements [

	elements add: (DynamicEyeElement 
			host: self object 
			label: 'ast' 
			value: [ self object ast ]).
			
	elements add: (DynamicEyeElement 
			host: self object 
			label: 'ir' 
			description: [ :value | | string |
				string := value longPrintString.
				string copyFrom: 2 to: string size ]
			value: [ self object ir ]).

	elements add: (DynamicEyeElement 
			host: self object 
			label: 'all bytecodes' 
			value: [ self object symbolic ]).

	elements add: (DynamicEyeElement 
			host: self object 
			label: 'header' 
			value: [ self object headerDescription ]).
			
	self object literals withIndexDo: [ :literal :index|
		elements add: (DynamicEyeElement 
				host: self object 
				label: 'literal ', index asString 
				value: literal) ].
			
]

{ #category : #list }
EyeCompiledMethodInspector >> addVariableFields: elements [
	
	self object initialPC to: self object size do: [ :index |
		elements add: (DynamicEyeElement 
				host: self object 
				label: 'bc ', index asString 
				value: (self object at: index) asString)].
]

{ #category : #accessing }
EyeCompiledMethodInspector >> title [
	^ self object printString 
]
