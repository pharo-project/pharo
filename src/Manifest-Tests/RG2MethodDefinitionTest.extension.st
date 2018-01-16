Extension { #name : #RG2MethodDefinitionTest }

{ #category : #'*Manifest-Tests' }
RG2MethodDefinitionTest >> testArrayStringForManifest [
	| method |
	method := thisContext method.

 	self
		assert: method asRing2Definition arrayStringForManifest 
		equals: { #RG2MethodDefinition . { self class name asSymbol . method selector . #false } }
]

{ #category : #'*Manifest-Tests' }
RG2MethodDefinitionTest >> testManifestReadOn [
	| method |
	method := thisContext method.

 	self
		assert: (RG2MethodDefinition manifestReadOn: {
			self class name asSymbol .
			method selector .
			#false }) name
		equals: method asRing2Definition name
]

{ #category : #'*Manifest-Tests' }
RG2MethodDefinitionTest >> testReadFrom [
	"self debug: #testReadFrom"
	
	
	| st rg read |
	rg := (Point>>#x) asRing2Definition.
	st := String streamContents: [:s |
		rg storeOn: s.
		s contents].
	read := (Object readFrom: st).
	self assert: read name equals: rg name.	
	self assert: read parent name equals: rg parent name.
	self assert: read isMetaSide equals: rg isMetaSide.	
]

{ #category : #'*Manifest-Tests' }
RG2MethodDefinitionTest >> testStoreOn [

	| st |
	st := String streamContents: [:s |
		
		(Point>>#x) asRing2Definition storeOn: s.
		s contents].
	self assert: st equals: '(RG2MethodDefinition className: ''Point'' selector: #x isMetaSide: false)'
]
