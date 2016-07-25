accessing
classRules

	^ self allRules select: [ :rule | rule class checksClass ]