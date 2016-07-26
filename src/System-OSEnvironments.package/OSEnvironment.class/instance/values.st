accessing
values
	"Answer a Collection containing the receiver's values."
	^ Array streamContents: [ :stream|
		self valuesDo: [ :value | stream nextPut: value ]]