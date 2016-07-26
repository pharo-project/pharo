private
info
	"For the testing purpose only; we use it in #instancesForAllEvents.
	We want to instantiate info only when it is really necessary."
	^ info ifNil: [ info := Dictionary new ].