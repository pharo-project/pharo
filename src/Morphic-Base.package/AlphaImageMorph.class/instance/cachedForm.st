accessing
cachedForm
	"Answer the value of cachedForm"
	
	|form i effectiveAlpha|
	cachedForm ifNil: [
		i := self scaledImage.
		effectiveAlpha := self enabled
			ifTrue: [self alpha]
			ifFalse: [self alpha / 2].
		effectiveAlpha = 1.0
			ifTrue: [self cachedForm: i]
			ifFalse: [form := Form extent: i extent depth: 32.
					form fillColor: (Color white alpha: 0.003922).
					(form getCanvas asAlphaBlendingCanvas: effectiveAlpha)
						drawImage: i
					at: 0@0.
					self cachedForm: form]].
	^cachedForm