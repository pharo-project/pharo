private
removeClassDefinitionWithoutCheckingMethods: aClass
	"Same than 'removeClassDefinition', excepts that it does not make any check to set defined methods as extensions"
	
	| removed aClassName|
	aClassName := aClass theNonMetaClass name.
	removed := classes remove: aClassName ifAbsent: [nil].
	removed ifNotNil: [
		self unregisterClass: aClass.] 
	