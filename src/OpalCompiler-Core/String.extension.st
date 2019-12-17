Extension { #name : #String }

{ #category : #'*OpalCompiler-Core' }
String >> parseLiterals [
	^ self class compiler parseLiterals: self
]
