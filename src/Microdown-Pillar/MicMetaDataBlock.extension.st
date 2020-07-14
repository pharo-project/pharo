Extension { #name : #MicMetaDataBlock }

{ #category : #'*Microdown-Pillar' }
MicMetaDataBlock >> asPillar [
	"Right now pillar does not represent metadata as PR object but as plain ston 
	objects. This is clearly not well done. It means that for the conversion to pillar we have no clue 
	what to. May be we should drop MicMetaDataBlock altogether"
	
	^ PRText new text: self printString
	
]
