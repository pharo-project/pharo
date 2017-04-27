*renraku
glmCritiqueSourceIn: composite

	^ composite pharoScript  
		title: 'Source';
		display: [ self definition ];
		act: [ self browse ] icon: GLMUIThemeExtraIcons glamorousBrowse entitled: 'Browse'