private
computeDays
	"Populate the days instance variable with CalendarChooserDay instances for the receiver's month."

	| cellHeight cellWidth height topLeft lastMonth nextMonth theDay thisMonth |
	topLeft := 0 @ 25.
	height := self bounds height - 25.
	cellHeight := height // 8.
	height := height - cellHeight.
	cellWidth := self bounds width // 7.
	lastMonth := date month previous.
	thisMonth := date month.
	nextMonth := date month next.
	days := OrderedCollection new.
	1 to: 6 do: [:lineIndex |
		| yOffset |
		yOffset := topLeft y + (lineIndex * cellHeight).
		(self daysForLine: lineIndex) withIndexDo: [:day :dayIndex |
			| cellPosX dayDate |
			dayDate := thisMonth asDate addDays: day - 1.
			(lineIndex = 1 and: [day > 7])
				ifTrue: [dayDate := lastMonth asDate addDays: day - 1].
			(lineIndex > 4 and: [day < 15])
				ifTrue: [dayDate := nextMonth asDate addDays: day - 1].
			cellPosX := cellWidth * (dayIndex - 1).
			days add: (theDay := CalendarDayMorph on: dayDate for: self).
			theDay bounds: (cellPosX @ yOffset extent: cellWidth @ cellHeight)]]