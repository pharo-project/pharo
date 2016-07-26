accessing
reset
	"self reset"
	| c |
	Current ifNil: [ ^self ].
	c := Current.
	Current := nil.
	c unregister.
