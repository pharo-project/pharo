instance creation
year: year week: week day: dayOfWeek

	^ self starting: (Week year: year week: week) start + (dayOfWeek - 1) days