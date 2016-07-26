private
withExampleDo: aBlock ifAbsent: anotherBlock
	^ [ aBlock value ]
		on: Error
		do: anotherBlock