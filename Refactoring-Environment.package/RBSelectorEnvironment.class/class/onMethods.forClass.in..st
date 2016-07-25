instance creation
onMethods: selectorCollection forClass: aClass in: anEnvironment 
	| environemnt |
	environemnt := self onEnvironment: anEnvironment.
	selectorCollection do: [ :each | environemnt addClass: aClass selector: each ].
	^ environemnt