accessing
height
	| count |
	count := self class maxLength.
	self visible ifTrue: [ 
		| height |
		height := World height - self bounds topLeft y.
		World submorphs do: [ :each |
			each class = TaskbarMorph 
				ifTrue: [ height := height - each height ] ].
		count := height // self class itemHeight - 1 min: count ].
	^ count asInteger isZero 
		ifTrue: [ 1 ]
		ifFalse: [ count ]