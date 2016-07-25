private
handleHit: association
	| link |
	statistics addHit.
	link := association value.
	self promote: link.
	^ link value value