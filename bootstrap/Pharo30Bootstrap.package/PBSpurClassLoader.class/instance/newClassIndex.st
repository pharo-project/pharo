as yet unclassified
newClassIndex
	
	nextClassIndex ifNil: [ nextClassIndex := 1024 ].
	^ nextClassIndex := nextClassIndex + 1