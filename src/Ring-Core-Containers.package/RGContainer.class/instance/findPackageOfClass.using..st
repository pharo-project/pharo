image package loading
findPackageOfClass: aRGBehaviorDefinition using: packageKeys

	"Look for the package of a class. It is nil when there is not a package created for a category in MC"
	| pair |
	self packages isEmpty ifTrue: [ ^ nil ].
	^ self packages at: aRGBehaviorDefinition category ifAbsent:[
		pair := packageKeys detect:[ :each| self class category: aRGBehaviorDefinition category matches: each key ] ifNone:[ nil ].
		pair ifNotNil:[ self packages at: pair key ] ]