ConfigurationOfAtomicFoo
linearProjectMethodSource

	^(self class sourceCodeAt: #project) asString copyReplaceAll: 'atomic' with: 'linear'

