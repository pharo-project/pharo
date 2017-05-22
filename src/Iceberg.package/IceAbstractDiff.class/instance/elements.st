accessing
elements
	^ elements ifNil: [ 
		elements := self initialElements sorted: [ :a :b | a description < b description ] ]