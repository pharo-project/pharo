tests
testSharedReferences
	| oneDotOne  point1 point2 materialized array |
	
	oneDotOne := 1.1.
	
	"I cannot directly use 4.2 because the Compiler reuses the same float instance 4.2 for the literals and hence the last assert fails. "
	point1:= Point x: oneDotOne y: (Float readFrom: '4.2' readStream).
	point2:= Point x: (Float readFrom: '4.2' readStream) y: oneDotOne.
	array := (Array with: point1 with: point2).
	
	materialized := self resultOfSerializeAndMaterialize: array.
	self assert: array = materialized.
	self assert: materialized first x == materialized second y.
	self deny: materialized first y == materialized second x.
	

	
	