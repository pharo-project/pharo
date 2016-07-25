protocol
updateEnabled
	"Update the enablement state."

	self model ifNotNil: [:m | 
		self getEnabledSelector ifNotNil: [:s |
			self enabled: (m perform: s)]]