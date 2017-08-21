accessing tags
tags
	| packageTag |
	"Any class or trait could be tagged by multiple symbols for user purpose.
	For now we only define API to manage them on top of RPackageTag"
	packageTag := self package classTagForClass: self.
	packageTag ifNil: [ ^#() ].
	packageTag isRoot ifTrue: [ ^#() ].
	
	^{packageTag name}