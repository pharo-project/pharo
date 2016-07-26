accessing
firstThursday
	"Return the years first Thursday.
	According to the ISO 8601 definition, the first week starts with the years first Thursday (week day number 5)"
	 ^ self start + (5 - self start dayOfWeek) days