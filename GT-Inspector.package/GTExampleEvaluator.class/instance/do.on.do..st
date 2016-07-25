private
do: aBlock on: anException do: anotherBlock
	aBlock
		on: anException
		do: anotherBlock