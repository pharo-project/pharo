private
run: aBlock
	
	[ GTExampleOrganizer instance stop. aBlock value ]
		ensure: [ GTExampleOrganizer instance start ].
	
	self verify