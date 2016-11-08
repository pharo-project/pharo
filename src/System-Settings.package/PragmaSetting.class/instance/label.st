accessing
label
	^ label ifNil: [self name translated] ifNotNil: [label translated]