Extension { #name : 'RGElementDefinition' }

{ #category : '*Tool-Base' }
RGElementDefinition >> browse [

	^ self tools browser openOnClass: self realClass
]
