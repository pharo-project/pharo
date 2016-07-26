private
find: aClass in: aMorph
	"Explores aMorph hierarchy of sub morphs to find a morph whose class inherits from aClass"
	^ self findSatisfying: [:morph | morph isKindOf: aClass ] in: aMorph  