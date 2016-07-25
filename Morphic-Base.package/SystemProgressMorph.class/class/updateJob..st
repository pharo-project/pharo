job subscription
updateJob: aJobChange
	| bars |
	bars := self uniqueInstance bars.
	bars isEmpty 
		ifFalse: [ 
			bars last 
				label: aJobChange title;
				progress: aJobChange progress ]