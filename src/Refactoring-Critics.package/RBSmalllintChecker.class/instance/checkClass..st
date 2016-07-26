private
checkClass: aClass 
	(environment includesClass: aClass) ifTrue: [rule checkClass: aClass]