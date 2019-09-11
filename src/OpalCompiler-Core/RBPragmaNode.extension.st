Extension { #name : #RBPragmaNode }

{ #category : #'*opalcompiler-core' }
RBPragmaNode >> asPragma [
	^ Pragma
		selector: selector
		arguments: (arguments collect: [ :each | each value ]) asArray
]

{ #category : #'*opalcompiler-core' }
RBPragmaNode >> asPrimitive [
	| args module name spec  |
	args := (self arguments collect: [ :each | each value ]) asArray.
	self isPrimitive
		ifFalse: [ IRPrimitive null ].
	args first isString
		ifTrue: [ 
			name := args first.
			module := self argumentAt: #module: ifAbsent: [ nil ].
			spec := {(module ifNotNil: [ module value asSymbol ]). (name asSymbol). 0. 0}.
			^ IRPrimitive new
				num: 117;
				spec: spec;
				yourself ]
		ifFalse: [ 
			^ IRPrimitive new
				num: args first;
				spec: nil;
				yourself ]
]

{ #category : #'*opalcompiler-core' }
RBPragmaNode >> isPrimitiveError [
	^ #( primitive:error: primitive:module:error: primitive:error:module:) includes: self selector
]
