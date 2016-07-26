private
dependenciesFilteredBy: aClass
	"Filter dependencies to only get Class instances"
	^ (dependencies select: [ :each | each class =  aClass ]) sorted