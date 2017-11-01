Extension { #name : #RBMultiEnvironment }

{ #category : #'*Manifest-Core' }
RBMultiEnvironment >> isMultiEnvironment [
	^ true
]

{ #category : #'*Manifest-Core' }
RBMultiEnvironment >> smallLintCritics [
	^ self asSelectorEnvironment smallLintCritics
]
