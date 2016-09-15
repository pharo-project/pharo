*OpalCompiler-Core
ir

	^ self propertyAt: #ir ifAbsent: [ self parent methodOrBlockNode ir ]