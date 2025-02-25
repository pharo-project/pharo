Extension { #name : 'RGMethodDefinition' }

{ #category : '*Tool-Base' }
RGMethodDefinition >> browse [

	^ (self tools toolNamed: #browser)
		  openOnClass: self realClass
		  selector: (self isDefined
				   ifTrue: [ self selector ]
				   ifFalse: [ nil ])
]
