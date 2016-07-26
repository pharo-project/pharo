class changes
trimHistory 
	"Drop non-essential history:  methods added and then removed, as well as rename and reorganization of newly-added classes."

	changeRecords do: [:chgRecord | chgRecord trimHistory]