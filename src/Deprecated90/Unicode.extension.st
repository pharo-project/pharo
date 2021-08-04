Extension { #name : #Unicode }

{ #category : #'*Deprecated90' }
Unicode class >> isJapanese: code [

	^ code > 255 and: [(JISX0208 charFromUnicode: code) notNil].

]

{ #category : #'*Deprecated90' }
Unicode class >> isKorean: code [

	^ code > 255 and: [(KSX1001 charFromUnicode: code) notNil]


]

{ #category : #'*Deprecated90' }
Unicode class >> ucsTable [
	^ Latin1 ucsTable 
]
