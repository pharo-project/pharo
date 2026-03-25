Extension { #name : 'CompiledMethod' }

{ #category : '*General-Rules' }
CompiledMethod >> hasEquivalentMethodInSuperclass [

	^ self overriddenMethod
		  ifNil: [ false ]
		  ifNotNil: [ :method | self equivalentTo: method ]
]
