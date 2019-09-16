Extension { #name : #CmCommandGroup }

{ #category : #'*Spec2-Commander2' }
CmCommandGroup >> asSpecGroup [
	"Subclasses might override this method to define default icon."
	^ self decorateWith: SpCommandGroup
]

{ #category : #'*Spec2-Commander2' }
CmCommandGroup >> asSpecGroupWithIconNamed: aSymbol [
	^ self asSpecGroup
		iconName: aSymbol;
		yourself
]

{ #category : #'*Spec2-Commander2' }
CmCommandGroup class >> forSpec [
	^ self new asSpecGroup
]

{ #category : #'*Spec2-Commander2' }
CmCommandGroup class >> forSpecWithIconNamed: aSymbol [
	^ self new asSpecGroupWithIconNamed: aSymbol
]
