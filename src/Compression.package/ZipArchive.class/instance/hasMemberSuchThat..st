accessing
hasMemberSuchThat: aBlock
	"Answer whether we have a member satisfying the given condition"
	^self members anySatisfy: aBlock