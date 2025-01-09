Extension { #name : 'OCPragmaNode' }

{ #category : '*OpalCompiler-Core' }
OCPragmaNode >> asIRPrimitive [
	"return a IRPrimitive for this method to be used in the compiler intermediate representation"
	| args module name spec |
	args := (self arguments collect: [ :each | each value ]) asArray.
	"normal methods have primitive number 0"
	self isPrimitive ifFalse: [ ^ OCIRPrimitive null ].
	^ args first isString
		ifTrue: [ name := args first.
			module := self argumentAt: #module: ifAbsent: [ nil ].
			spec := {(module ifNotNil: [ module value asSymbol ]) . name asSymbol . 0 . 0}.
			OCIRPrimitive new
				num: 117;
				spec: spec;
				yourself ]
		ifFalse: [
			"these are standard numbered primitives"
			OCIRPrimitive new
				num: args first;
				spec: nil;
				yourself ]
]

{ #category : '*OpalCompiler-Core' }
OCPragmaNode >> asPragma [
	^ Pragma
		selector: selector
		arguments: (arguments collect: [ :each | each value ]) asArray
]

{ #category : '*OpalCompiler-Core' }
OCPragmaNode >> isPrimitiveError [
	^ #( primitive:error: primitive:module:error: primitive:error:module:) includes: self selector
]

{ #category : '*OpalCompiler-Core' }
OCPragmaNode >> primitiveErrorVariableName [
	^(self argumentAt: #error:) value
]
