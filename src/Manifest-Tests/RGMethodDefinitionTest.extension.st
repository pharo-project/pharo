Extension { #name : #RGMethodDefinitionTest }

{ #category : #'*Manifest-Tests' }
RGMethodDefinitionTest >> testArrayStringForManifest [
	| method |
	method := thisContext method.

 	self
		assert: method asRingDefinition arrayStringForManifest
		equals: { #RGMethodDefinition . { self class name asSymbol . method selector . #false } }
]

{ #category : #'*Manifest-Tests' }
RGMethodDefinitionTest >> testManifestReadOn [
	| method |
	method := thisContext method.

 	self
		assert: (RGMethodDefinition manifestReadOn: {
			self class name asSymbol .
			method selector .
			#false })
		equals: method asRingDefinition
]

{ #category : #'*Manifest-Tests' }
RGMethodDefinitionTest >> testReadFrom [
	
	| st rg |
	rg := (Point>>#x) asRingDefinition.
	st := String streamContents: [:s |
		
		rg storeOn: s.
		s contents].
	self assert: (Object readFrom: st) = rg 
]

{ #category : #'*Manifest-Tests' }
RGMethodDefinitionTest >> testStoreOn [

	| st |
	st := String streamContents: [:s |
		
		(Point>>#x) asRingDefinition storeOn: s.
		s contents].
	self assert: st = '(RGMethodDefinition className: ''Point'' selector: #x isMetaSide: false)'
]
