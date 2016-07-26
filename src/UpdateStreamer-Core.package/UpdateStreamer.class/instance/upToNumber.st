accessing
upToNumber
	"When specified the update will stop at this number"
	
	^ upToNumber ifNil: [ upToNumber := 1000000 ].