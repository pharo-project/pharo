testing
classIncludesSelector
	
	| aClass | 
	^ (aClass := self methodClass) notNil
			and: [aClass includesSelector: self methodSelector]