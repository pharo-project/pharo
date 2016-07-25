private
addSubclassesOf: aClass in: result withIndex: index

	| classes |
	classes := aClass subclasses "self selectedPackage
					ifNil: [ {} ]
					ifNotNil: [:p | aClass subclasses intersection: p classes ]".

	classes do: [:each |
		result at: each put: index.
		self addSubclassesOf: each theNonMetaClass in: result withIndex: index + 1 ]