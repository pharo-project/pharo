Extension { #name : 'RGElementDefinition' }

{ #category : '*Tools' }
RGElementDefinition >> browse [

	^ (self tools toolNamed: #browser) openOnClass: self realClass
]
