accessing
findFirst: aBlock
	"Note, this method only for backward compatibility. It duplicating the semantics of #nextOrNilSuchThat: completely.
	Use #nextOrNilSuchThat: instead "

	
	^ self nextOrNilSuchThat: aBlock
	