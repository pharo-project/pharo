running
basicCheck: aClass 
	^ ((aClass includesSelector: #=) and: [ (aClass includesSelector: #hash) not ])