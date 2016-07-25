settings
applySettingsFrom: aThemeSettings
	"Change the settings to be those of aThemeSettings."
	
	self
		windowColor: aThemeSettings windowColor;
		selectionColor: aThemeSettings selectionColor;
		autoSelectionColor: aThemeSettings autoSelectionColor