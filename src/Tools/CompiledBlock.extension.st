Extension { #name : 'CompiledBlock' }

{ #category : '*Tools' }
CompiledBlock >> browse [
	| method |

	method := self method.
	
	method isInstalled
		ifTrue: [
			^ Smalltalk tools browser
				openOnClass: self methodClass
				selector: self selector
				highlight: self sourceNode sourceCode ]
		ifFalse: [
			^ Smalltalk tools browser
				openOnClass: self methodClass
				selector: self selector ]

]
