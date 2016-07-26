tests
testIndexOfDay
	| days |
	days := #(#Sunday #Monday #Tuesday #Wednesday #Thursday #Friday #Saturday).
	
	days withIndexDo: [:item :index | self assert: (Week indexOfDay: item) = index].
	
	"This should probably raise an error rather than returning 0."
	self assert: (Week indexOfDay: 0) = 0.
	self assert: (Week indexOfDay: 1) = 0.
	self assert: (Week indexOfDay: 7) = 0.
	self assert: (Week indexOfDay: 8) = 0.
	self assert: (Week indexOfDay: #Sunnyday) = 0.