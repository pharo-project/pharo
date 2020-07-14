Extension { #name : #Object }

{ #category : #'*renraku' }
Object >> externalProperties [

	| pragmas  externalProperties |
	pragmas := Pragma allNamed: #eProperty from: self class to: ProtoObject.
	externalProperties := OrderedCollection new.

	pragmas do: [ :pragma |
		[ externalProperties addAll:
			(pragma method
				valueWithReceiver:
				self arguments: #())
		]
			on: Exception
			do: [ :ex |
				ReExceptionStrategy current
					handle: ex
					about: self
					forProppertiesDo: [ :prop |
						externalProperties add: prop ]  ] ].
			
	^ externalProperties

]

{ #category : #'*renraku' }
Object >> glmCritiqueSourceIn: composite [

	"display nothing for now"
]
