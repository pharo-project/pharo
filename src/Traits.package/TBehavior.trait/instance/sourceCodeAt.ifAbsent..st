accessing method dictionary
sourceCodeAt: selector ifAbsent: aBlock

	^ (self compiledMethodAt: selector ifAbsent: [^ aBlock value]) sourceCode.