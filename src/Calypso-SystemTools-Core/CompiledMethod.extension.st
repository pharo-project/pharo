Extension { #name : #CompiledMethod }

{ #category : #'*Calypso-SystemTools-Core' }
CompiledMethod >> astForStylingInCalypso [
	"this method simplified remote method styling. 
	Remote proxy will redefine it by building AST on client side"
	^self ast
]
