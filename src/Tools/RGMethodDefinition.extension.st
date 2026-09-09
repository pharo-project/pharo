Extension { #name : 'RGMethodDefinition' }

{ #category : '*Tools' }
RGMethodDefinition >> browse [

	^ (self tools toolNamed: #browser)
		  openOnClass: self realClass
		  selector: (self isDefined
				   ifTrue: [ self selector ]
				   ifFalse: [ nil ])
]
