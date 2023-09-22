Extension { #name : 'RGBehaviorDefinition' }

{ #category : '*Tool-Base' }
RGBehaviorDefinition >> browse [

	^ self tools browser openOnClass: self realClass
]
