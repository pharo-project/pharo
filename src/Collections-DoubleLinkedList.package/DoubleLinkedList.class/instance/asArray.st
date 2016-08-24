converting
asArray
	"Return a new Array containing my elements"
	
	^ Array streamContents: [ :out |
		self do: [ :each | out nextPut: each ] ]