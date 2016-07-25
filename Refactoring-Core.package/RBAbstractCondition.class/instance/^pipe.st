logical operations
| aCondition 
	"(A | B) = (A not & B not) not"

	^(self not & aCondition not) not