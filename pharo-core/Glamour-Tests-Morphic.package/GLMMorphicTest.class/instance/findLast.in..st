private
findLast: aClass in: aMorph
	"Explores aMorph hierarchy of sub morphs to find the last morph whose class inherits from aClass"
	^ self findLastSatisfying: [:morph | morph isKindOf: aClass ] in: aMorph  