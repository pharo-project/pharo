Extension { #name : 'PackageTag' }

{ #category : '*Tools' }
PackageTag >> browse [

	^ Smalltalk tools browser openWithState: [ :browser |
		  browser selectPackage: self package andTag: self name ]
]
