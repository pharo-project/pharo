query
allMethods
	^self allBehaviors flatCollect: [ :behavior | behavior methods]
