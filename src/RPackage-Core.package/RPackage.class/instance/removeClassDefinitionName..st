class addition
removeClassDefinitionName: aClassName
	"remove the class definition from the package but not its method"
	
	| removed aClassNameSymbol |
	('* class' match: aClassName) ifTrue: [^ self error: 'no metaclass name'].
	aClassNameSymbol := aClassName asSymbol.
	"clean up class tags"
	classTags do: [ :tag| tag removeClassNamed: aClassName ].
	(classTags select: #isEmpty)
		do: [ :eachTag | self basicRemoveTag: eachTag ].
	"remove the class definition"
	removed := classes remove: aClassNameSymbol ifAbsent: [nil].
	removed ifNotNil: [
		self definedMethodsShouldBecomeExtensionWhenRemovingClass: aClassName.
		self unregisterClassName: aClassNameSymbol ] 