enumerating
between: aStart and: anEnd do: aBlock
	"from Cuis 99"
	
	| element end i startDate |
	end := self end min: anEnd.

	element := self start.	
	"Performance optimization. Avoid going through unnecesary days if easy."
	startDate := aStart asDate.
	(startDate > element asDate and: [ self everyDayAtSameTimes ]) ifTrue: [
		element := DateAndTime date: startDate time: element asTime ].

	i := 1.
	[ element < aStart ] whileTrue: [
		element := element + (schedule at: i).
		i := i + 1.
		i > schedule size ifTrue: [i := 1]].

	[ element <= end ] whileTrue: [
		aBlock value: element.
		element := element + (schedule at: i).
		i := i + 1.
		i > schedule size ifTrue: [i := 1]]
.
