Extension { #name : #Metaclass }

{ #category : #'*Ring-Core' }
Metaclass >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ RGMetaclass named: self name parent: anRGEnvironment.

]
