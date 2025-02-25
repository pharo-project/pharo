Extension { #name : 'RGBehaviorDefinition' }

{ #category : '*Tool-Base' }
RGBehaviorDefinition >> browse [

	^ (self tools toolNamed: #browser) openOnClass: self realClass
]
