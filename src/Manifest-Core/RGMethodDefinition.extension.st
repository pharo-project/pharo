Extension { #name : 'RGMethodDefinition' }

{ #category : '*Manifest-Core' }
RGMethodDefinition >> arrayStringForManifest [
	| array |
	array := Array new: 2.
	array at: 1 put: (self class asString asSymbol).
	array at: 2 put: {self parentName asSymbol. self selector asSymbol. self isMeta asString asSymbol}.
	^ array
]

{ #category : '*Manifest-Core' }
RGMethodDefinition class >> manifestReadOn: aArray [

 	^ self className: (aArray at: 1)
		selector: (aArray at: 2)
		isMeta: ((aArray at: 3) = #true)
]

{ #category : '*Manifest-Core' }
RGMethodDefinition >> storeOn: aStream [
	aStream nextPut: $(.
	aStream nextPutAll: self class asString.
	aStream nextPutAll: ' className: '''.
	aStream nextPutAll: self parentName.
	aStream nextPutAll: ''' selector: '.
	aStream nextPutAll: self selector printString.
	aStream nextPutAll: ' isMeta: '.
	aStream nextPutAll: self isMeta printString.
	aStream nextPut: $)
]
