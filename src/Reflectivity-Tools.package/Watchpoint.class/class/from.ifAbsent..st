accessing
from: aNode ifAbsent: aBlock
	^self allWatchpoints at: aNode ifAbsent: aBlock.