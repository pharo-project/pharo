Extension { #name : 'RGBehaviorDefinition' }

{ #category : '*Tools' }
RGBehaviorDefinition >> browse [

	^ (self tools toolNamed: #browser) openOnClass: self realClass
]
