Extension { #name : #MicCodeBlock }

{ #category : #'*Microdown-Pillar' }
MicCodeBlock >> asPillar [
	self flag: #fixme. "missing arguments"
	
	^ PRCodeblock new
		language: self firstTag;
		text: self code;
		yourself
]
