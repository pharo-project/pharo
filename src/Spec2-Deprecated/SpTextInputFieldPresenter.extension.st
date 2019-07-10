Extension { #name : #SpTextInputFieldPresenter }

{ #category : #'*Spec2-Deprecated' }
SpTextInputFieldPresenter >> actionToPerformHolder [
	self deprecated: 'Spec should not provide a way to get its internals. actionToPerform is not a value holder anymore but an observable slot, thus, this method cannot return anything anymore.'
]

{ #category : #'*Spec2-Deprecated' }
SpTextInputFieldPresenter >> beDecrypted [
	self deprecated: 'Use #beText instead' transformWith: '`@receiver beDecrypted' -> '`@receiver beText'.
	self beText
]

{ #category : #'*Spec2-Deprecated' }
SpTextInputFieldPresenter >> beEncrypted [
	self deprecated: 'Use #bePassword instead' transformWith: '`@receiver beEncrypted' -> '`@receiver bePassword'.

	self bePassword
]

{ #category : #'*Spec2-Deprecated' }
SpTextInputFieldPresenter >> encrypted [
	self deprecated: 'Use #isPassword instead' transformWith: '`@receiver encrypted' -> '`@receiver isPassword'.
	^ self isPassword
]

{ #category : #'*Spec2-Deprecated' }
SpTextInputFieldPresenter >> encrypted: aBoolean [
	self deprecated: 'Use #bePassword: instead' transformWith: '`@receiver encrypted: `@arg' -> '`@receiver bePassword: `@arg'.
	self bePassword: aBoolean
]

{ #category : #'*Spec2-Deprecated' }
SpTextInputFieldPresenter >> ghostText [
	self deprecated: 'This API is too tight to Morph. Use #placeholder instead.' transformWith: '`@receiver ghostText' -> '`@receiver placeholder'.
	^ self placeholder
]

{ #category : #'*Spec2-Deprecated' }
SpTextInputFieldPresenter >> ghostText: aText [
	self deprecated: 'This API is too tight to Morph. Use #placeholder: instead.' transformWith: '`@receiver ghostText: `@statement' -> '`@receiver placeholder: `@statement'.
	self placeholder: aText
]

{ #category : #'*Spec2-Deprecated' }
SpTextInputFieldPresenter >> textHolder [
	self deprecated: 'Nobody should access directly to internals of Spec. Now the text is an observable slot and not a value holder.'.
]

{ #category : #'*Spec2-Deprecated' }
SpTextInputFieldPresenter >> whenAcceptOnCRChanged: aBlock [
	self deprecated: 'Use #whenAcceptOnCRChangedDo: instead.' transformWith: '`@receiver whenAcceptOnCRChanged: `@statements' -> '`@receiver whenAcceptOnCRChangedDo: `@statements'.
	self whenAcceptOnCRChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpTextInputFieldPresenter >> whenEntryCompletionChanged: aBlock [
	self deprecated: 'Use #whenEntryCompletionChangedDo: instead.' transformWith: '`@receiver whenEntryCompletionChanged: `@statements' -> '`@receiver whenEntryCompletionChangedDo: `@statements'.
	self whenEntryCompletionChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpTextInputFieldPresenter >> whenGhostTextChanged: aBlock [
	self deprecated: 'This API is too tight to Morph. Use #whenPlaceholderChanged: instead.' transformWith: '`@receiver whenGhostTextChanged: `@statement' -> '`@receiver whenPlaceholderChanged: `@statement'.
	self whenPlaceholderChangedDo: aBlock
]
