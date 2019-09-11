"
I am EyeFloatInspector.
I am an EyeInspector.

I am a specialized Inspector for Floats.
I add extra elements (sign, significand, exponent) of the Float object that I am inspecting.

Note that these should be interpreted as

sign * significand * (2 raisedToInteger: exponent)
"
Class {
	#name : #EyeFloatInspector,
	#superclass : #EyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #list }
EyeFloatInspector >> addSpecialFields: elements [

	elements add: (DynamicEyeElement 
		host: self object 
		label: 'sign' 
		value: [ self object sign ]).
			
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'significand' 
		value: [ self object significand ]).
			
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'exponent' 
		value: [ self object exponent ]).

	elements add: (DynamicEyeElement
		host: self object
		label: 'binary'
		value: [ self object binaryLiteralString ]).

]
