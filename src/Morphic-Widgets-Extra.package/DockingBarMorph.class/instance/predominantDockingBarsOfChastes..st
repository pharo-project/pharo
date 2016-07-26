private - accessing
predominantDockingBarsOfChastes: predominantChastes 
	"Private - Answer a collection of the docking bar of my owner  
	that are predominant to the receiver.  
	 
	By 'predominant' we mean docking bar that have the right to  
	get a position before the receiver.  
	 
	The predominance of individual living in the same chaste is  
	determinated by the arrival order. "

	| allDockingBars byChaste byArrival |
	(self owner isNil or: [self owner isHandMorph]) ifTrue: [^ #()].

	allDockingBars := self owner dockingBars.

	byChaste := allDockingBars select: [:each | predominantChastes includes: each edgeToAdhereTo].
	(predominantChastes includes: self edgeToAdhereTo) ifFalse: [^ byChaste].
	byChaste := byChaste reject: [:each | each edgeToAdhereTo = self edgeToAdhereTo].	
	byArrival := allDockingBars select: [:each | each edgeToAdhereTo = self edgeToAdhereTo].
	byArrival := byArrival copyAfter: self.
	^ byChaste , byArrival