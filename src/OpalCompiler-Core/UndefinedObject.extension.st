Extension { #name : #UndefinedObject }

{ #category : #'*opalcompiler-core' }
UndefinedObject >> allTempNames [
	^#()
]

{ #category : #'*OpalCompiler-Core' }
UndefinedObject >> compiler [
	"nil is used as a class for when compiling expressions (for exampke DoIts)"
	^Smalltalk compiler
]
