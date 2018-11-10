Extension { #name : #IceOperation }

{ #category : #'*Iceberg-TipUI' }
IceOperation >> browse [
	
	^ self leftDefinition browse
]

{ #category : #'*Iceberg-TipUI' }
IceOperation >> canBeBrowsed [
	
	^ self leftDefinition canBeBrowsed
]

{ #category : #'*Iceberg-TipUI' }
IceOperation >> canBrowseReferences [
	
	^ false
]

{ #category : #'*Iceberg-TipUI' }
IceOperation >> icon [
	
	self subclassResponsibility
]

{ #category : #'*Iceberg-TipUI' }
IceOperation >> preferredColor [

	"Use the default color"
	^ nil
]
