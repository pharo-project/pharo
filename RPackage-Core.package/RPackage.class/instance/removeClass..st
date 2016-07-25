accessing
removeClass: aClass
	"Remove the class and all its methods from the receiver. If we have a protocol which looks like an extension of us, rename it to 'as yet unclassified' to avoid breaking things afterwards."

	aClass organization categories
		do: [ :each | 
			(self isYourClassExtension: each)
				ifTrue: [ aClass organization renameCategory: each toBe: Protocol defaultName ] ].
	(self definedMethodsForClass: aClass instanceSide) do: [ :aCompiledMethod | self removeMethod: aCompiledMethod ].
		"we also have also have to remove methods from class side"
	(self definedMethodsForClass: aClass classSide) do: [ :aCompiledMethod | self removeMethod: aCompiledMethod ].
	self removeClassDefinitionWithoutCheckingMethods: aClass.
	self removeClassTagsForClassNamed: aClass name