*OpalCompiler-Core
ir

	^ self propertyAt: #ir ifAbsent: [self generateIR].