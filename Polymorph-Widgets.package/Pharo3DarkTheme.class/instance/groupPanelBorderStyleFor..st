border-styles
groupPanelBorderStyleFor: aGroupPanel
	"Answer the normal border style for a group panel."

	^ SimpleBorder new
		width: 1;
		baseColor: ((self glamorousBaseColorFor: aGroupPanel))