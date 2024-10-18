Extension { #name : 'Object' }

{ #category : '*System-Settings-Core' }
Object >> acceptSettings: aVisitor [

	^ aVisitor visitObject: self
]

{ #category : '*System-Settings-Core' }
Object class >> exportSettingAction [
	
	^ self
]

{ #category : '*System-Settings-Core' }
Object >> exportSettingAction [

	^ self class exportSettingAction
]
