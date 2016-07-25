instance creation
newWith: aPackageCycle named: aString
	
	| generator |
	generator := self new.
	generator packageCycle: aPackageCycle.
	generator nameOfGraph: aString.
	generator buildGraphViz.
	^ generator
	