walking
do: aBlock
	[ self hasNext ] whileTrue: [ 
		aBlock value: self next
	]
