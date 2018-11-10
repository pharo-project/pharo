Extension { #name : #UIManager }

{ #category : #'*Iceberg-Plugin-GitHub' }
UIManager >> inform: aString actionOnClick: aBlock [
	"Only the MorphicUIManager should make the growl clickable."
	
	^ self inform: aString
]
