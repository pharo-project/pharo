Extension { #name : #SpApplication }

{ #category : #'*Spec2-Dialogs' }
SpApplication >> newConfirm [

	^ SpConfirmDialog newApplication: self
]

{ #category : #'*Spec2-Dialogs' }
SpApplication >> newInform [

	^ SpInformDialog newApplication: self
]

{ #category : #'*Spec2-Dialogs' }
SpApplication >> newRequest [

	^ SpRequestDialog newApplication: self
]

{ #category : #'*Spec2-Dialogs' }
SpApplication >> newRequestText [

	^ SpRequestTextDialog newApplication: self
]

{ #category : #'*Spec2-Dialogs' }
SpApplication >> newSelect [

	^ SpSelectDialog newApplication: self
]
