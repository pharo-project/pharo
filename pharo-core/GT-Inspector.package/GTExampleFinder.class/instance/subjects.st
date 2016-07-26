accessing
subjects
	^ subjects ifNil: [ 
		source 
			ifNotNil: [ OrderedCollection with: source ]
			ifNil: [ OrderedCollection new ] ].