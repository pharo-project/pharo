accessing
current
	"singleton pattern"
	Current ifNil: [ 
		Current := self new.
		Current register ].
	^Current 