SmalltalkImage classPool at: #SpecialSelectors put: (Smalltalk specialObjectsArray at:24).

Class subclasses: (Array with: ProtoObject class).
ProtoObject class classLayout slotScope parentScope: Class classLayout slotScope.

RBProgramNode classPool at: #FormatterClass put: BISimpleFormatter.