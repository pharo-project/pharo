instance creation
fromDateAndTime: aDateAndTime 

	| dateValue timeValue local |
	local := aDateAndTime asLocal.
	dateValue := self dateValueFrom: local asDate.
	timeValue := self timeValueFrom: local asTime.
	^ self on: (dateValue << 16) + timeValue.