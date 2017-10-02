temporary-patch
fixPharoClassInstallerMethodCopying

	"By default the class installer tries to recompile existing code when loading a new version of a class.
	Since the initial bootstrap has no sources, this will fail.
	This patch creates new methods by copying them and fixing the corresponding literals"
	ShiftClassBuilder compile: 'compileMethods
	oldClass localMethods do: [ :m | | copy |
		copy := m copy.
		copy methodClass: newClass.
		copy literals withIndexDo: [:l :i |
			(l isKindOf: ClassVariable)
				ifTrue: [ | newLiteral |
					newLiteral := newClass innerBindingOf: l key.
					m literalAt: i put: newLiteral ].
		].
		newClass addSelectorSilently: copy selector withMethod: copy.
	].
	oldClass classSide localMethods do: [ :m | | copy |
		copy := m copy.
		copy methodClass: newClass classSide.
		copy literals withIndexDo: [:l :i |
			(l isKindOf: ClassVariable)
				ifTrue: [ | newLiteral |
					newLiteral := newClass innerBindingOf: l key.
					m literalAt: i put: newLiteral ].
		].
		newClass classSide addSelectorSilently: copy selector withMethod: copy.
	].'.