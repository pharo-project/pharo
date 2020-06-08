Extension { #name : #CompiledBlock }

{ #category : #'*Slot-Core' }
CompiledBlock >> removeSavedTemp: aTemporaryVariable [ 
	
	^ self method removeSavedTemp: aTemporaryVariable
]

{ #category : #'*Slot-Core' }
CompiledBlock >> saveTemp: aTemp [

	^ self method saveTemp: aTemp
]

{ #category : #'*Slot-Core' }
CompiledBlock >> savedTemps [ 

	^ self method savedTemps
]
