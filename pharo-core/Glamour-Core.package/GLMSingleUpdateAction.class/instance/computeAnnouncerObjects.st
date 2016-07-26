public
computeAnnouncerObjects 
	self flag: 'We catch the error because if there is a problem in the computation of the announcer object, we still want to be able to continue the execution'.
	^ OrderedCollection with: 
		([self transformation glamourValue: self presentation entity] 
			on: Error 
			do: [:e | self presentation entity. e resume])