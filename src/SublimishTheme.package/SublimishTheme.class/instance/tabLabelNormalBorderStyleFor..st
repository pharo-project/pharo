border-styles
tabLabelNormalBorderStyleFor: aTabLabel
"	^SimpleBorder new
		width: 0;
		baseColor: (self buttonBaseColorFor: aTabLabel) darker
"	^ self buttonNormalBorderStyleFor: aTabLabel