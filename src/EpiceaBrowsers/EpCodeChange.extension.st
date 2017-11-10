Extension { #name : #EpCodeChange }

{ #category : #'*EpiceaBrowsers' }
EpCodeChange >> applyCodeChange [
	"Apply this code change in current image"

	^ self accept: EpApplyVisitor new

]

{ #category : #'*EpiceaBrowsers' }
EpCodeChange >> asRevertedCodeChange [
	"Answer a code change that represents my inverse operation. For example, a class addition will answer a class removal."

	^ self accept: EpInverseVisitor new

]

{ #category : #'*EpiceaBrowsers' }
EpCodeChange >> previewedApplyEvents [

	^ self accept: EpApplyPreviewer new
]
