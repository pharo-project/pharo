testing
isNilUUID
	"Return true when I am equal to the nilUUID, 00000000-0000-0000-0000-000000000000, false otherwise"
	
	^ self allSatisfy: [ :each | each isZero ]