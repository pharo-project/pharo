*renraku
reverseNodesDo: aBlock 
	self children reverseDo: [ :each | each reverseNodesDo: aBlock ].
	aBlock value: self