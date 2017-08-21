system changes
managersForCategory: aSystemCategory do: aBlock
	"Got to be careful here - we might get method categories where capitalization is problematic."
	| cat foundOne index |
	foundOne := false.
	cat := aSystemCategory ifNil:[^nil]. "yes this happens; for example in eToy projects"
	"first ask PackageInfos, their package name might not match the category"
	self
		bestMatchingManagerForCategory: aSystemCategory
		do: [ :mgr |
			aBlock value: mgr.
			foundOne := true ].

   foundOne ifTrue: [^self].
	["Loop over categories until we found a matching one"
	self registry at: (MCPackage named: cat) ifPresent:[:mgr|
		aBlock value: mgr.
		foundOne := true.
	].
	index := cat lastIndexOf: $-.
	index > 0]whileTrue:[
		"Step up to next level package"
		cat := cat copyFrom: 1 to: index-1.
	].
	foundOne ifFalse:[
		"Create a new (but only top-level)"
		aBlock value: (MCWorkingCopy forPackage: (MCPackage named: aSystemCategory capitalized)).
	].