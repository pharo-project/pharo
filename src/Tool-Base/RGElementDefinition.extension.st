Extension { #name : 'RGElementDefinition' }

{ #category : '*Tool-Base' }
RGElementDefinition >> browse [

	^ (self tools toolNamed: #browser) openOnClass: self realClass
]
