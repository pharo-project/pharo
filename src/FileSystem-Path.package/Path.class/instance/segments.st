accessing
segments
	"return an array with all the path segements separated"
	| segments |
	
	segments := OrderedCollection new.
	
	self do: [ :part|
		segments add: part
	].
	
	^ segments asArray 