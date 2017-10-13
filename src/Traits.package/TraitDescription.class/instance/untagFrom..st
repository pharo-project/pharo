accessing tags
untagFrom: aSymbol
	| package packageTag |
	"Any class or trait could be tagged by multiple symbols for user purpose.
	For now we could only model single tag with RPackageTag"
	package := self package.
	packageTag := package classTagForClass: self.
	packageTag ifNil: [ ^#() ].
	packageTag isRoot ifTrue: [ ^#() ].
	packageTag name = aSymbol ifFalse: [ ^self ].
	packageTag removeClass: self.
	package addClass: self