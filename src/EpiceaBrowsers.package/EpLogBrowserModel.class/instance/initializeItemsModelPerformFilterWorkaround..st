initialization
initializeItemsModelPerformFilterWorkaround: widgetBuildAnnouncement
	"Problem: FastTableModel sends #explicitFilter to the FTTableMorph in MorphicFastTableAdapter>>buildWidget but the EpEntryItem in the itemsModel are not compatible."
	
	widgetBuildAnnouncement widget disableFunction