private - accessing
usedHeightByPredominantDockingBarsOfChastes: predominantChastes 
	"Private - convenience"
	^(self predominantDockingBarsOfChastes: predominantChastes)
		ifEmpty: [0]
		ifNotEmpty: [:predominants | (predominants collect: [:each | each height]) sum] 