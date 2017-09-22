private
calculateHighestUpdate 
	|result|
	result := (major * 10000) + (minor * 1000).
	self hasValidBuildNumber ifTrue: [ result := result + build asNumber ].
	^result 
	
