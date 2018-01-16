Extension { #name : #RG2MethodDefinition }

{ #category : #'*Manifest-Core' }
RG2MethodDefinition >> arrayStringForManifest [
	| array |
	array := Array new:2.
	array at: 1 put: (self class name asSymbol).
	array at: 2 put: {self parent name asSymbol.  self name asSymbol. self isMetaSide asString  asSymbol}.
	^ array 
	 
]

{ #category : #'*Manifest-Core' }
RG2MethodDefinition class >> manifestReadOn: aArray [

 	^ self className:  (aArray at: 1) 
		selector: (aArray at: 2)
		isMetaSide: ((aArray at: 3) = #true)   
		
]

{ #category : #'*Manifest-Core' }
RG2MethodDefinition >> storeOn: aStream [
	aStream nextPut: $(.
	aStream nextPutAll: self class asString.
	aStream nextPutAll: ' className: '''.
	aStream nextPutAll: self parent name.
	aStream nextPutAll: ''' selector: '.
	aStream nextPutAll: self selector printString.
	aStream nextPutAll: ' isMetaSide: '.
	aStream nextPutAll: self isMetaSide printString.
	aStream nextPut: $)
]
