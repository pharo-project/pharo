Extension { #name : #Clipboard }

{ #category : #'*Iceberg-TipUI' }
Clipboard class >> clipboardText: aText informing: aString [
	UIManager inform: aString.
	^ self clipboardText: aText
]
