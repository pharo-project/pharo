Extension { #name : 'Object' }

{ #category : '*System-Settings-Core' }
Object >> acceptSettings: aVisitor [

	^ aVisitor visitObject: self
]

{ #category : '*System-Settings-Core' }
Object class >> settingDeclarationDefault [

	^ nil
]
